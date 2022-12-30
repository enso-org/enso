//! This module implements `World`, the main object responsible for handling what you see on the
//! screen.

use crate::control::callback::traits::*;
use crate::data::dirty::traits::*;
use crate::display::render::*;
use crate::prelude::*;
use crate::system::web::traits::*;

use crate::animation;
use crate::application::command::FrpNetworkProvider;
use crate::control::callback;
use crate::data::dirty;
use crate::debug;
use crate::debug::stats::Stats;
use crate::debug::stats::StatsData;
use crate::display;
use crate::display::garbage;
use crate::display::render;
use crate::display::render::passes::SymbolsRenderPass;
use crate::display::scene;
use crate::display::scene::DomPath;
use crate::display::scene::Scene;
use crate::display::shape::primitive::glsl;
use crate::system::web;

use enso_types::unit2::Duration;
use web::prelude::Closure;
use web::JsCast;
use web::JsValue;


// ==============
// === Export ===
// ==============

pub use crate::display::symbol::types::*;



// ================
// === Uniforms ===
// ================

/// Uniforms managed by world.
#[derive(Clone, CloneRef, Debug)]
pub struct Uniforms {
    time:         Uniform<f32>,
    display_mode: Uniform<i32>,
}

impl Uniforms {
    /// Constructor.
    pub fn new(scope: &UniformScope) -> Self {
        let time = scope.add_or_panic("time", 0.0);
        let display_mode = scope.add_or_panic("display_mode", 0);
        Self { time, display_mode }
    }
}


// =========================
// === Metadata Profiler ===
// =========================

profiler::metadata_logger!("RenderStats", log_render_stats(StatsData));



// =============
// === World ===
// =============

/// The root object for EnsoGL scenes.
#[derive(Clone, CloneRef, Debug, Default)]
pub struct World {
    rc: Rc<WorldDataWithLoop>,
}

impl World {
    /// Constructor.
    pub fn new() -> Self {
        Self::default()
    }

    /// Constructor modifier. Displays the default scene in the provided path.
    pub fn displayed_in(self, dom: impl DomPath) -> Self {
        self.default_scene.display_in(dom);
        self
    }

    /// Keeps the world alive even when all references are dropped. Use only if you want to keep one
    /// instance of the world forever.
    pub fn keep_alive_forever(&self) {
        mem::forget(self.clone_ref())
    }
}

impl Deref for World {
    type Target = WorldDataWithLoop;
    fn deref(&self) -> &Self::Target {
        &self.rc
    }
}

impl display::Object for World {
    fn display_object(&self) -> &display::object::Instance {
        self.default_scene.display_object()
    }
}

impl<'t> From<&'t World> for &'t Scene {
    fn from(world: &'t World) -> Self {
        &world.default_scene
    }
}


// ===========
// === FRP ===
// ===========

crate::define_endpoints_2! {
    Output {
        after_rendering(),
    }
}


// =========================
// === WorldDataWithLoop ===
// =========================

/// Main loop closure type.
pub type MainLoop = animation::Loop;

/// World data with a main loop implementation.
///
/// # Main Loop Performance
/// Any code repeated on each iteration of the Main Loop (each "frame") must be written with a high
/// care for performance. Any changes that has a chance of negatively impacting the constant
/// overhead of the main loop needs *explicit* explanation, review, and acceptance *at the design
/// stage* of the proposed new implementation, from performance perspective, with an
/// explicit note of the fact of Main Loop impact.
///
/// Rationale: the "Main Loop" contains the code comprising a GUI rendering "frame" (term
/// originating from a "still frame" term in filmmaking). The speed at which the Main Loop executes
/// directly translates to the perceived performance of the GUI, and the FPS (frames per second)
/// metric, impacting Users' experience with the application.
#[derive(Debug)]
pub struct WorldDataWithLoop {
    frp:  Frp,
    data: WorldData,
}

impl WorldDataWithLoop {
    /// Constructor.
    pub fn new() -> Self {
        let frp = Frp::new();
        let data = WorldData::new(&frp.private.output);
        let on_frame_start = animation::on_frame_start();
        let on_before_rendering = animation::on_before_rendering();
        let network = frp.network();
        crate::frp::extend! {network
            eval on_frame_start ((t) data.run_stats(*t));
            eval on_before_rendering ((t) data.run_next_frame(*t));
        }

        Self { frp, data }
    }
}

impl Default for WorldDataWithLoop {
    fn default() -> Self {
        Self::new()
    }
}

impl Deref for WorldDataWithLoop {
    type Target = WorldData;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}



// =================
// === Callbacks ===
// =================

// FIXME[WD]: move these callbacks to the FRP interface one day.
/// Callbacks that are run during rendering of the frame.
#[derive(Clone, CloneRef, Debug, Default)]
#[allow(missing_docs)]
pub struct Callbacks {
    pub prev_frame_stats: callback::registry::Ref1<StatsData>,
    pub before_frame:     callback::registry::Copy1<animation::TimeInfo>,
    pub after_frame:      callback::registry::Copy1<animation::TimeInfo>,
}



// ======================
// === Scene Instance ===
// ======================

thread_local! {
    /// Global scene reference. See the [`scene`] function to learn more.
    pub static SCENE: RefCell<Option<Scene>> = RefCell::new(None);
}

/// Get reference to [`Scene`] instance. This should always succeed. Scenes are managed by [`World`]
/// and should be instantiated before any callback is run.
pub fn scene() -> Scene {
    SCENE.with_borrow(|t| t.clone().unwrap())
}


// ============================
// === Static Shape Systems ===
// ============================

type ShapeCons = Box<dyn Fn() -> Box<dyn crate::gui::component::AnyShapeView>>;

thread_local! {
    /// All shapes defined with the `shape!` macro. They will be populated on the beginning of
    /// program execution, before the `main` function is called.
    pub static STATIC_SHAPES: RefCell<Vec<ShapeCons>> = default();
}

thread_local! {
    pub static PRECOMPILED_SHADERS: RefCell<HashMap<&'static str, String>> = default();
}



// =================
// === WorldData ===
// =================

/// The data kept by the [`World`].
#[derive(Debug, Clone, CloneRef, Deref)]
#[allow(missing_docs)]
pub struct WorldData {
    #[deref]
    frp:                  api::private::Output,
    pub default_scene:    Scene,
    scene_dirty:          dirty::SharedBool,
    uniforms:             Uniforms,
    display_mode:         Rc<Cell<glsl::codes::DisplayModes>>,
    stats:                Stats,
    stats_monitor:        debug::monitor::Monitor,
    stats_draw_handle:    callback::Handle,
    pub on:               Callbacks,
    debug_hotkeys_handle: Rc<RefCell<Option<web::EventListenerHandle>>>,
    garbage_collector:    garbage::Collector,
}

impl WorldData {
    /// Create and initialize new world instance.
    pub fn new(frp: &api::private::Output) -> Self {
        // FIXME: describe
        scene::with_symbol_registry(|_| {});
        warn!(">>0");
        PRECOMPILED_SHADERS.with_borrow_mut(|t| t.insert("lib/rust/ensogl/example/auto-layout/src/lib.rs:36:5", "
    highp vec2 _2666 = abs(input_local.xy);
    highp vec2 _2651 = (-input_size) * vec2(0.5) + _2666;
    highp vec2 _2672 = min(_2651, vec2(0.0));
    highp float _3104 = input_z_zoom_1 / input_local.z;
    highp float _3088 = 0.5 / _3104;
    highp vec2 _3160 = vec2(input_size.x, input_size.y);
    highp float _3464 = min(clamp(input_size.x * 0.0500000007450580596923828125, 0.0, 1.0), clamp(input_size.y * 0.0500000007450580596923828125, 0.0, 1.0));
    highp vec2 _3269 = _3160 * vec2(0.5);
    highp float _3273 = _3269.x;
    highp float _3276 = _3464 * 10.0 + (-_3273);
    bool _3277 = input_local.x < _3276;
    bool _3287;
    if (_3277)
    {
        _3287 = input_local.y > ((-_3464) * 10.0 + _3269.y);
    }
    else
    {
        _3287 = _3277;
    }
    highp float _7237 = -_3464;
    highp float _3293 = _7237 * 10.0 + _3273;
    bool _3294 = input_local.x > _3293;
    bool _3304;
    if (_3294)
    {
        _3304 = input_local.y > (_7237 * 10.0 + _3269.y);
    }
    else
    {
        _3304 = _3294;
    }
    bool _3323;
    if (_3277)
    {
        _3323 = input_local.y < (_3464 * 10.0 + (-_3269.y));
    }
    else
    {
        _3323 = _3277;
    }
    bool _3341;
    if (_3294)
    {
        _3341 = input_local.y < (_3464 * 10.0 + (-_3269.y));
    }
    else
    {
        _3341 = _3294;
    }
    highp float _7157;
    if (_3287)
    {
        _7157 = _7237 * 10.0 + length(input_local.xy - vec2(_3276, _7237 * 10.0 + _3269.y));
    }
    else
    {
        highp float _7158;
        if (_3304)
        {
            _7158 = _7237 * 10.0 + length(input_local.xy - vec2(_3293, _7237 * 10.0 + _3269.y));
        }
        else
        {
            highp float _7159;
            if (_3323)
            {
                _7159 = _7237 * 10.0 + length(input_local.xy - vec2(_3276, _3464 * 10.0 + (-_3269.y)));
            }
            else
            {
                highp float _7160;
                if (_3341)
                {
                    _7160 = _7237 * 10.0 + length(input_local.xy - vec2(_3293, _3464 * 10.0 + (-_3269.y)));
                }
                else
                {
                    highp vec2 _3417 = (-_3160) * vec2(0.5) + _2666;
                    _7160 = min(max(_3417.x, _3417.y), 0.0) + length(max(_3417, vec2(0.0)));
                }
                _7159 = _7160;
            }
            _7158 = _7159;
        }
        _7157 = _7158;
    }
    highp float _3953 = 0.20000000298023223876953125 * clamp(((-_7157) * input_pixel_ratio + _3088) * _3104, 0.0, 1.0);
    highp float _4068 = 7.787036895751953125 * 0.0 + 0.13973100483417510986328125;
    highp float _4081 = step(0.0, 0.008856450207531452178955078125);
    highp float _4087 = mix(0.0, _4068, _4081);
    highp float _4032 = _4087 - _4087;
    highp float _4033 = 500.0 * _4032;
    highp float _4039 = 200.0 * _4032;
    highp float _4310 = max(_7157, max(_2672.x, _2672.y) + length(max(_2651, vec2(0.0))));
    highp vec4 _4392 = vec4(vec4(max(0.0, 116.0 * _4087 + (-16.0)), length(vec2(_4033, _4039)), atan(_4039, _4033), _3953).xyz * _3953, _3953) * clamp(((-_4310) * input_pixel_ratio + _3088) * _3104, 0.0, 1.0);
    highp float _2267 = _4392.w;
    bool _2270 = _2267 > 0.0;
    highp float _2271 = float(_2270);
    highp float _4455 = (input_global_instance_id > 16777215) ? 0.3921568691730499267578125 : _2271;
    highp vec3 _4459 = vec3(clamp(float(uint(input_global_instance_id & 255)) * 0.0039215688593685626983642578125, 0.0, 1.0), clamp(float(uint((input_global_instance_id & 65280) >> 8)) * 0.0039215688593685626983642578125, 0.0, 1.0), clamp(float(uint((input_global_instance_id & 16711680) >> 16)) * 0.0039215688593685626983642578125, 0.0, 1.0)) * _4455;
    output_id = vec4(_4459, _4455);
    if (input_display_mode == 0)
    {
        highp vec3 _7191;
        if (_2270)
        {
            _7191 = _4392.xyz / vec3(_2267);
        }
        else
        {
            _7191 = _4392.xyz;
        }
        highp float _4761 = _7191.x + 16.0;
        highp float _4762 = 0.008620689623057842254638671875 * _4761;
        highp float _4769 = 0.008620689623057842254638671875 * _4761 + (0.00200000009499490261077880859375 * (cos(_7191.z) * _7191.y));
        highp float _4783 = 0.008620689623057842254638671875 * _4761 + ((sin(_7191.z) * _7191.y) * (-0.004999999888241291046142578125));
        output_color = vec4(vec3(mix((_4769 * _4769) * _4769, 0.128418505191802978515625 * (_4769 - 0.13973100483417510986328125), step(_4769, 0.2068965435028076171875)), mix((_4762 * _4762) * _4762, 0.128418505191802978515625 * (0.008620689623057842254638671875 * _4761 + (-0.13973100483417510986328125)), step(_4762, 0.2068965435028076171875)), mix((_4783 * _4783) * _4783, 0.128418505191802978515625 * (_4783 - 0.13973100483417510986328125), step(_4783, 0.2068965435028076171875))) * mat3(vec3(3.240600109100341796875, -1.53719997406005859375, -0.498600006103515625), vec3(-0.968900024890899658203125, 1.87580001354217529296875, 0.0414999984204769134521484375), vec3(0.0557000003755092620849609375, -0.203999996185302734375, 1.05700004100799560546875)), _2267);
        highp vec4 _2296 = output_color;
        highp vec3 _2298 = _2296.xyz * _2267;
        output_color.x = _2298.x;
        output_color.y = _2298.y;
        output_color.z = _2298.z;
    }
    else
    {
        if (input_display_mode == 5)
        {
            highp vec3 _7190;
            if (_2270)
            {
                _7190 = _4392.xyz / vec3(_2267);
            }
            else
            {
                _7190 = _4392.xyz;
            }
            highp float _5039 = _7190.x + 16.0;
            highp float _5040 = 0.008620689623057842254638671875 * _5039;
            highp float _5047 = 0.008620689623057842254638671875 * _5039 + (0.00200000009499490261077880859375 * (cos(_7190.z) * _7190.y));
            highp float _5061 = 0.008620689623057842254638671875 * _5039 + ((sin(_7190.z) * _7190.y) * (-0.004999999888241291046142578125));
            output_color = vec4(vec3(mix((_5047 * _5047) * _5047, 0.128418505191802978515625 * (_5047 - 0.13973100483417510986328125), step(_5047, 0.2068965435028076171875)), mix((_5040 * _5040) * _5040, 0.128418505191802978515625 * (0.008620689623057842254638671875 * _5039 + (-0.13973100483417510986328125)), step(_5040, 0.2068965435028076171875)), mix((_5061 * _5061) * _5061, 0.128418505191802978515625 * (_5061 - 0.13973100483417510986328125), step(_5061, 0.2068965435028076171875))) * mat3(vec3(3.240600109100341796875, -1.53719997406005859375, -0.498600006103515625), vec3(-0.968900024890899658203125, 1.87580001354217529296875, 0.0414999984204769134521484375), vec3(0.0557000003755092620849609375, -0.203999996185302734375, 1.05700004100799560546875)), _2267);
            highp vec4 _2318 = output_color;
            highp vec3 _2320 = _2318.xyz * _2267;
            output_color.x = _2320.x;
            output_color.y = _2320.y;
            output_color.z = _2320.z;
            bool _5201 = input_uv.x < 0.0;
            bool _5208;
            if (!_5201)
            {
                _5208 = input_uv.x > 1.0;
            }
            else
            {
                _5208 = _5201;
            }
            bool _5215;
            if (!_5208)
            {
                _5215 = input_uv.y < 0.0;
            }
            else
            {
                _5215 = _5208;
            }
            bool _5222;
            if (!_5215)
            {
                _5222 = input_uv.y > 1.0;
            }
            else
            {
                _5222 = _5215;
            }
            output_color = mix(output_color, vec4(1.0, 0.0, 0.0, 1.0), bvec4(_5222));
        }
        else
        {
            if (input_display_mode == 4)
            {
                highp float _2345 = (200.0 / _3104) * input_pixel_ratio;
                highp float _5283 = log(20.0 / _2345) * 0.4342944622039794921875;
                highp float _5365 = floor(_5283);
                highp float _5406 = clamp(log(1.0 + abs(_4310 / _2345)), 0.0, 1.0);
                highp float _5448 = clamp(sqrt(_5406), 0.0, 1.0);
                highp vec3 _5479 = max(vec3(0.0), clamp(vec3(sqrt(_5448), (_5448 * _5448) * _5448, max(sin(5.497786998748779296875 * _5448), pow(_5448, 12.0))), vec3(0.0), vec3(1.0)) * (2.0 * _5406 + 0.5));
                highp vec3 _7189;
                if (sign(_4310) < 0.0)
                {
                    _7189 = _5479.yxz * 3.0;
                }
                else
                {
                    _7189 = _5479;
                }
                highp float _5310 = _4310 * 6.283185482025146484375;
                highp float _5513 = fract(_5283);
                highp vec3 _5551 = (_7189 * ((0.89999997615814208984375 * (1.0 - mix(pow(0.5 * cos(_5310 * pow(10.0, _5365)) + 0.5, 10.0), 0.0, smoothstep(0.5, 1.0, _5513)))) * (1.0 - mix(0.0, pow(0.5 * cos(_5310 * pow(10.0, _5365 + 1.0)) + 0.5, 10.0), smoothstep(0.0, 0.5, _5513))) + 0.100000001490116119384765625)) * 2.0;
                highp vec3 _5555 = _5551 * 0.1500000059604644775390625;
                output_color = vec4(pow((((_5551 * (_5555 + vec3(0.0500000007450580596923828125)) + vec3(0.0040000001899898052215576171875)) / (_5551 * (_5555 + vec3(0.5)) + vec3(0.060000002384185791015625))) - vec3(0.066666662693023681640625)) * vec3(1.3790643215179443359375), vec3(0.4545454680919647216796875)), 1.0);
                output_color.w = _2271;
                highp vec4 _2362 = output_color;
                highp vec3 _2364 = _2362.xyz * _2271;
                output_color.x = _2364.x;
                output_color.y = _2364.y;
                output_color.z = _2364.z;
            }
            else
            {
                if (input_display_mode == 6)
                {
                    highp vec3 _5700 = clamp(abs((fract(vec3(float(((input_global_instance_id + (input_mouse_click_count % 10)) * 7) % 100) * 0.00999999977648258209228515625, 1.0, 0.5).xxx + vec3(1.0, 0.666666686534881591796875, 0.3333333432674407958984375)) * 6.0) - vec3(3.0)) - vec3(1.0), vec3(0.0), vec3(1.0)) * 0.5;
                    output_color.x = _5700.x;
                    output_color.y = _5700.y;
                    output_color.z = _5700.z;
                    output_color.w = _2271;
                    highp vec4 _2409 = output_color;
                    highp vec3 _2411 = _2409.xyz * _2271;
                    output_color.x = _2411.x;
                    output_color.y = _2411.y;
                    output_color.z = _2411.z;
                }
                else
                {
                    if (input_display_mode == 2)
                    {
                        output_color = vec4(clamp(abs((fract(vec4(mod(float(input_global_instance_id + (input_mouse_click_count % 10)) * 17.0, 100.0) * 0.00999999977648258209228515625, 1.0, 1.0, _2271).xxx + vec3(1.0, 0.666666686534881591796875, 0.3333333432674407958984375)) * 6.0) - vec3(3.0)) - vec3(1.0), vec3(0.0), vec3(1.0)) * 1.0, _2271);
                        bool _5864 = input_uv.x < 0.0;
                        bool _5871;
                        if (!_5864)
                        {
                            _5871 = input_uv.x > 1.0;
                        }
                        else
                        {
                            _5871 = _5864;
                        }
                        bool _5878;
                        if (!_5871)
                        {
                            _5878 = input_uv.y < 0.0;
                        }
                        else
                        {
                            _5878 = _5871;
                        }
                        bool _5885;
                        if (!_5878)
                        {
                            _5885 = input_uv.y > 1.0;
                        }
                        else
                        {
                            _5885 = _5878;
                        }
                        highp vec4 _7188;
                        if (_5885)
                        {
                            _7188 = vec4(output_color.xyz, 1.0);
                        }
                        else
                        {
                            _7188 = output_color;
                        }
                        output_color = _7188;
                        output_color.w *= clamp(float(input_mouse_position.x) * 0.001000000047497451305389404296875, 0.100000001490116119384765625, 1.0);
                        highp float _2472 = output_color.w;
                        highp vec4 _2473 = output_color;
                        highp vec3 _2475 = _2473.xyz * _2472;
                        output_color.x = _2475.x;
                        output_color.y = _2475.y;
                        output_color.z = _2475.z;
                    }
                    else
                    {
                        if (input_display_mode == 3)
                        {
                            highp float _2495 = float(((input_global_instance_id + (input_mouse_click_count % 10)) * 7) % 100) * 0.00999999977648258209228515625;
                            highp vec4 _6102 = vec4(clamp(abs((fract(vec3(_2495, 1.0, 0.60000002384185791015625).xxx + vec3(1.0, 0.666666686534881591796875, 0.3333333432674407958984375)) * 6.0) - vec3(3.0)) - vec3(1.0), vec3(0.0), vec3(1.0)) * 0.60000002384185791015625, 1.0);
                            output_color = vec4(clamp(abs((fract(vec3(_2495, 1.0, 0.800000011920928955078125).xxx + vec3(1.0, 0.666666686534881591796875, 0.3333333432674407958984375)) * 6.0) - vec3(3.0)) - vec3(1.0), vec3(0.0), vec3(1.0)) * 0.800000011920928955078125, clamp(float(input_mouse_position.x) * 0.00999999977648258209228515625, 0.100000001490116119384765625, 1.0));
                            output_color *= _2271;
                            highp float _6153 = (((min(1.0, _3104) / input_pixel_ratio) + 0.100000001490116119384765625) * 0.5) / _3104;
                            bool _6171 = (abs(mod(input_local.x + 0.5, 1.0) - 0.5) <= _6153) || (abs(mod(input_local.y + 0.5, 1.0) - 0.5) <= _6153);
                            bool _6176;
                            if (_6171)
                            {
                                bool _6248 = input_uv.x < 0.0;
                                bool _6255;
                                if (!_6248)
                                {
                                    _6255 = input_uv.x > 1.0;
                                }
                                else
                                {
                                    _6255 = _6248;
                                }
                                bool _6262;
                                if (!_6255)
                                {
                                    _6262 = input_uv.y < 0.0;
                                }
                                else
                                {
                                    _6262 = _6255;
                                }
                                bool _6269;
                                if (!_6262)
                                {
                                    _6269 = input_uv.y > 1.0;
                                }
                                else
                                {
                                    _6269 = _6262;
                                }
                                _6176 = !_6269;
                            }
                            else
                            {
                                _6176 = _6171;
                            }
                            highp vec4 _7169;
                            if (_6176)
                            {
                                highp vec4 _6188 = _6102 * clamp((_3104 - 3.0) * 0.25, 0.0, 1.0);
                                _7169 = _6188 + (output_color * (1.0 - _6188.w));
                            }
                            else
                            {
                                _7169 = output_color;
                            }
                            output_color = _7169;
                            highp float _6327 = (((min(2.0, _3104) / input_pixel_ratio) + 0.100000001490116119384765625) * 0.5) / _3104;
                            bool _6345 = (abs(mod(input_local.x + 1.0, 2.0) - 1.0) <= _6327) || (abs(mod(input_local.y + 1.0, 2.0) - 1.0) <= _6327);
                            bool _6350;
                            if (_6345)
                            {
                                bool _6422 = input_uv.x < 0.0;
                                bool _6429;
                                if (!_6422)
                                {
                                    _6429 = input_uv.x > 1.0;
                                }
                                else
                                {
                                    _6429 = _6422;
                                }
                                bool _6436;
                                if (!_6429)
                                {
                                    _6436 = input_uv.y < 0.0;
                                }
                                else
                                {
                                    _6436 = _6429;
                                }
                                bool _6443;
                                if (!_6436)
                                {
                                    _6443 = input_uv.y > 1.0;
                                }
                                else
                                {
                                    _6443 = _6436;
                                }
                                _6350 = !_6443;
                            }
                            else
                            {
                                _6350 = _6345;
                            }
                            highp vec4 _7178;
                            if (_6350)
                            {
                                highp vec4 _6362 = _6102 * clamp((_3104 - 2.0) * 0.3333333432674407958984375, 0.0, 1.0);
                                _7178 = _6362 + (output_color * (1.0 - _6362.w));
                            }
                            else
                            {
                                _7178 = output_color;
                            }
                            output_color = _7178;
                            highp float _6501 = (((min(4.0, _3104) / input_pixel_ratio) + 0.100000001490116119384765625) * 0.5) / _3104;
                            bool _6519 = (abs(mod(input_local.x + 2.0, 4.0) - 2.0) <= _6501) || (abs(mod(input_local.y + 2.0, 4.0) - 2.0) <= _6501);
                            bool _6524;
                            if (_6519)
                            {
                                bool _6596 = input_uv.x < 0.0;
                                bool _6603;
                                if (!_6596)
                                {
                                    _6603 = input_uv.x > 1.0;
                                }
                                else
                                {
                                    _6603 = _6596;
                                }
                                bool _6610;
                                if (!_6603)
                                {
                                    _6610 = input_uv.y < 0.0;
                                }
                                else
                                {
                                    _6610 = _6603;
                                }
                                bool _6617;
                                if (!_6610)
                                {
                                    _6617 = input_uv.y > 1.0;
                                }
                                else
                                {
                                    _6617 = _6610;
                                }
                                _6524 = !_6617;
                            }
                            else
                            {
                                _6524 = _6519;
                            }
                            highp vec4 _7187;
                            if (_6524)
                            {
                                highp vec4 _6536 = _6102 * clamp(_3104, 0.0, 1.0);
                                _7187 = _6536 + (output_color * (1.0 - _6536.w));
                            }
                            else
                            {
                                _7187 = output_color;
                            }
                            output_color = _7187;
                        }
                        else
                        {
                            if (input_display_mode == 1)
                            {
                                bool _2566 = input_uv.x >= 1.0;
                                bool _2573;
                                if (!_2566)
                                {
                                    _2573 = input_uv.y >= 1.0;
                                }
                                else
                                {
                                    _2573 = _2566;
                                }
                                output_color = vec4(input_uv, _2573 ? 0.5 : 0.0, 1.0);
                            }
                            else
                            {
                                bool _2598 = mod(input_local.x, input_size.x * 0.5) < (input_size.x * 0.25);
                                bool _2606 = mod(input_local.y, input_size.y * 0.5) < (input_size.y * 0.25);
                                bool _2610 = _2598 || _2606;
                                bool _2617;
                                if (_2610)
                                {
                                    _2617 = !(_2598 && _2606);
                                }
                                else
                                {
                                    _2617 = _2610;
                                }
                                output_color = mix(vec4(0.0, 0.0, 0.0, 1.0), vec4(1.0, 0.0, 0.0, 1.0), bvec4(_2617));
                            }
                        }
                    }
                }
            }
        }
    }
        ".to_string()));

        warn!(">>1");
        let frp = frp.clone_ref();
        warn!(">>2");
        warn!(">> {:?}", web::window);
        let stats = debug::stats::Stats::new(web::window.performance_or_panic());
        let stats_monitor = debug::monitor::Monitor::new();
        warn!(">>2");
        let on = Callbacks::default();
        warn!(">>3");
        let scene_dirty = dirty::SharedBool::new(());
        let on_change = enclose!((scene_dirty) move || scene_dirty.set());
        let display_mode = Rc::<Cell<glsl::codes::DisplayModes>>::default();
        warn!(">>2");
        let default_scene = Scene::new(&stats, on_change, &display_mode);
        let uniforms = Uniforms::new(&default_scene.variables);
        let debug_hotkeys_handle = default();
        let garbage_collector = default();
        let stats_draw_handle = on.prev_frame_stats.add(f!([stats_monitor] (stats: &StatsData) {
            stats_monitor.sample_and_draw(stats);
            log_render_stats(*stats)
        }));

        SCENE.with_borrow_mut(|t| *t = Some(default_scene.clone_ref()));

        Self {
            frp,
            default_scene,
            scene_dirty,
            uniforms,
            display_mode,
            stats,
            on,
            debug_hotkeys_handle,
            stats_monitor,
            stats_draw_handle,
            garbage_collector,
        }
        .init()
    }

    fn init(self) -> Self {
        self.init_environment();
        self.init_composer();
        self.init_debug_hotkeys();
        self
    }

    fn init_environment(&self) {
        init_global();
    }

    fn init_debug_hotkeys(&self) {
        let stats_monitor = self.stats_monitor.clone_ref();
        let display_mode = self.display_mode.clone_ref();
        let display_mode_uniform = self.uniforms.display_mode.clone_ref();
        let closure: Closure<dyn Fn(JsValue)> = Closure::new(move |val: JsValue| {
            let event = val.unchecked_into::<web::KeyboardEvent>();
            let digit_prefix = "Digit";
            if event.alt_key() && event.ctrl_key() {
                let key = event.code();
                if key == "Backquote" {
                    stats_monitor.toggle()
                } else if key == "KeyP" {
                    enso_debug_api::save_profile(&profiler::internal::take_log());
                } else if key == "KeyQ" {
                    enso_debug_api::save_profile(&profiler::internal::take_log());
                    enso_debug_api::LifecycleController::new().map(|api| api.quit());
                } else if key.starts_with(digit_prefix) {
                    let code_value = key.trim_start_matches(digit_prefix).parse().unwrap_or(0);
                    if let Some(mode) = glsl::codes::DisplayModes::from_value(code_value) {
                        warn!("Setting display mode to {:?}.", mode.name());
                        display_mode.set(mode);
                    } else {
                        warn!("Invalid display mode code: {code_value}.");
                    }
                    display_mode_uniform.set(code_value as i32);
                }
            }
        });
        let handle = web::add_event_listener_with_bool(&web::window, "keydown", closure, true);
        *self.debug_hotkeys_handle.borrow_mut() = Some(handle);
    }

    fn init_composer(&self) {
        let mouse_hover_rgba = self.default_scene.mouse.hover_rgba.clone_ref();
        let garbage_collector = &self.garbage_collector;
        let mut pixel_read_pass = PixelReadPass::<u8>::new(&self.default_scene.mouse.position);
        pixel_read_pass.set_callback(f!([garbage_collector](v) {
            mouse_hover_rgba.set(Vector4::from_iterator(v.iter().map(|value| *value as u32)));
            garbage_collector.pixel_updated();
        }));
        pixel_read_pass.set_sync_callback(f!(garbage_collector.pixel_synced()));
        // TODO: We may want to enable it on weak hardware.
        // pixel_read_pass.set_threshold(1);
        let logger = Logger::new("renderer");
        let pipeline = render::Pipeline::new()
            .add(SymbolsRenderPass::new(logger, &self.default_scene.layers))
            .add(ScreenRenderPass::new())
            .add(pixel_read_pass);
        self.default_scene.renderer.set_pipeline(pipeline);
    }

    fn run_stats(&self, time: Duration) {
        let previous_frame_stats = self.stats.begin_frame(time);
        if let Some(stats) = previous_frame_stats {
            self.on.prev_frame_stats.run_all(&stats);
        }
    }

    /// Perform to the next frame with the provided time information.
    ///
    /// Please note that the provided time information from the [`requestAnimationFrame`] JS
    /// function is more precise than time obtained from the [`window.performance().now()`] one.
    /// Follow this link to learn more:
    /// https://stackoverflow.com/questions/38360250/requestanimationframe-now-vs-performance-now-time-discrepancy.
    #[profile(Objective)]
    pub fn run_next_frame(&self, time: animation::TimeInfo) {
        self.on.before_frame.run_all(time);
        self.uniforms.time.set(time.since_animation_loop_started.unchecked_raw());
        self.scene_dirty.unset_all();
        let update_status = self.default_scene.update(time);
        self.garbage_collector.mouse_events_handled();
        self.default_scene.render(update_status);
        self.on.after_frame.run_all(time);
        self.stats.end_frame();
        self.after_rendering.emit(());
    }

    /// Pass object for garbage collection.
    ///
    /// The collector is designed to handle EnsoGL component's FRP networks and models, but any
    /// structure with static timeline may be put. For details, see docs of [`garbage::Collector`].
    #[profile(Debug)]
    pub fn collect_garbage<T: 'static>(&self, object: T) {
        self.garbage_collector.collect(object);
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn native_compilation_in_test_mode() {
        let _world = World::new().displayed_in("root");
        let _scene = &_world.default_scene;
    }
}

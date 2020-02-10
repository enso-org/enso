/// See the following link for more information about this color mapping:
/// https://gamedev.stackexchange.com/questions/62917/uncharted-2-tone-mapping-and-an-eye-adaptation
vec3 uncharted_to_tone_mapping(vec3 color) {
    float A = 0.15;
    float B = 0.50;
    float C = 0.10;
    float D = 0.20;
    float E = 0.02;
    float F = 0.30;
    float W = 11.2;
    float exposure = 2.;
    color *= exposure;
    color = ((color * (A * color + C * B) + D * E) / (color * (A * color + B) + D * F)) - E / F;
    float white = ((W * (A * W + C * B) + D * E) / (W * (A * W + B) + D * F)) - E / F;
    color /= white;
    return color;
}

/// The "fusion" gradient, going from dark magenta (0) to white (1).
/// Often seen as heatmap visualization in papers.
vec3 fusion_gradient(float x) {
    float t = clamp(x,0.0,1.0);
    return clamp(vec3(sqrt(t), t*t*t, max(sin(PI*1.75*t), pow(t, 12.0))), 0.0, 1.0);
}

// HDR version of the "fusion" gradient.
vec3 fusion_gradient_hdr(float x) {
    float t = clamp(x,0.0,1.0);
    return fusion_gradient(sqrt(t))*(0.5+2.*t);
}

// TODO: consider using (dFdx, dFdy) to detect discontinuities.
Rgb distance_meter(float dist, float ray_length, float cam_height) {
    float idealGridDistance = 20.0/ray_length;
    float nearestBase = floor(log(idealGridDistance)/log(10.));
    float relativeDist = abs(dist/cam_height);

    float largerDistance = pow(10.0,nearestBase+1.);
    float smallerDistance = pow(10.0,nearestBase);


    vec3 col = fusion_gradient_hdr(log(1.+relativeDist));
    col = max(vec3(0.),col);
    if (sign(dist) < 0.) {
        col = col.grb*3.;
    }

    float l0 = (pow(0.5+0.5*cos(dist*PI*2.*smallerDistance),10.0));
    float l1 = (pow(0.5+0.5*cos(dist*PI*2.*largerDistance),10.0));

    float x = fract(log(idealGridDistance)/log(10.));
    l0 = mix(l0,0.,smoothstep(0.5,1.0,x));
    l1 = mix(0.,l1,smoothstep(0.0,0.5,x));

    col.rgb *= 0.1+0.9*(1.-l0)*(1.-l1);
    col = uncharted_to_tone_mapping(col);
    col = pow(col, vec3(1./2.2));
    return rgb(col);
}

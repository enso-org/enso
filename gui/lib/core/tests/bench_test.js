export function set_gradient_bg(dom, r, g, b) {
  let components = r + "," + g + "," + b;
  let css = "radial-gradient(rgb(" + components + ") 50%,"
          + "rgba(" + components + ", 0.0))";
  dom.style.backgroundImage = css;
}

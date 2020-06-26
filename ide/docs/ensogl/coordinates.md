

## Spaces

- **Object Space**  
  Local object coordinates.
  
- **World Space `(world_matrix * object_space)`**  
  The position relatively to the origin of the world (point `(0,0)` below).  
  <img width="400" src="https://user-images.githubusercontent.com/1623053/85816645-37e00280-b76c-11ea-9831-e6ae7378830e.png"/>
  
- **Eye Space**  
  The position relatively to the placement of the camera.
  ```rust
  let view_matrix = camera_matrix.inverse();
  let eye_space   = view_matrix * world_space;
  ```
  <img width="400" src="https://user-images.githubusercontent.com/1623053/85816908-d40a0980-b76c-11ea-8be6-6c982b1d8ce5.png"/>
  
- **Clip Space**  
  The position inside of the Normalized Device Coordinates (NDC) cube. In perspective projection, a 3D point in a truncated 
  pyramid frustum (eye coordinates) is mapped to the NDC cube. The range of x-coordinate from `[l,r]` to `[-1,1]`, the 
  y-coordinate from `[b,t]` to `[-1,1]` and the z-coordinate from `[-n,-f]` to `[-1,1]`. Note that the eye coordinates are defined 
  in the right-handed coordinate system, but NDC uses the left-handed coordinate system. That is, the camera at the origin is 
  looking along -Z axis in eye space, but it is looking along +Z axis in NDC.
  ```rust
  let clip_space = projection_matrix * eye_space;
  ```

  <img width="600" src="https://user-images.githubusercontent.com/1623053/85817711-0caae280-b76f-11ea-9111-1357195cf580.png"/>
  <img width="600" src="https://user-images.githubusercontent.com/1623053/85817751-22b8a300-b76f-11ea-8f18-f3e78f3139c1.png"/>
  <img width="600" src="https://user-images.githubusercontent.com/1623053/85817783-3e23ae00-b76f-11ea-8972-c90f1eb6ba1e.png"/>



## Sources
Images and fragments used here are parts of the following articles:
- https://webglfundamentals.org/webgl/lessons/webgl-3d-camera.html
- http://www.songho.ca/opengl/gl_transform.html
- http://www.songho.ca/opengl/gl_projectionmatrix.html
- http://www.songho.ca/math/homogeneous/homogeneous.html

#version 450

layout (set = 0, binding = 0) uniform camera {
  mat4 projectionView;
};

layout(location = 0) in vec3 positionIn;

layout(location = 0) out vec3 texCoordOut;

void main() {
  gl_Position   = projectionView * vec4(positionIn, 1.0);
  gl_Position.z = 0;
  texCoordOut   = positionIn;
}

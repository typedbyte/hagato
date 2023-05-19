#version 450

layout (set = 0, binding = 0) uniform camera {
  mat4 projectionView;
};

layout(push_constant) uniform constants
{
  mat4 model;
};

layout(location = 0) in vec3 positionIn;
layout(location = 1) in vec3 normalIn;
layout(location = 2) in vec2 texCoordIn;

layout(location = 0) out vec4 positionOut;
layout(location = 1) out vec3 normalOut;
layout(location = 2) out vec2 texCoordOut;

void main() {
  positionOut = model * vec4(positionIn, 1.0);
  gl_Position = projectionView * positionOut;
  normalOut   = normalIn;
  texCoordOut = texCoordIn;
}

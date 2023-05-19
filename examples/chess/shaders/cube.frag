#version 450

layout(set = 1, binding = 1) uniform samplerCube[] skybox;

layout(location = 0) in vec3 texCoord;

layout(location = 0) out vec4 color;

void main() {
  color = texture(skybox[0], texCoord);
}

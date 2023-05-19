#version 450
#extension GL_EXT_nonuniform_qualifier : require

layout(set = 1, binding = 1) uniform sampler2D[] textures;

layout(push_constant) uniform constants
{
  layout(offset = 64) int texIndex;
  int focus;
};

layout(location = 0) in vec4 position;
layout(location = 1) in vec3 normal;
layout(location = 2) in vec2 texCoord;

layout(location = 0) out vec4 color;

const vec3 lightPosition = vec3(-20, 0, 20);
const vec3 lightColor    = vec3(1, 1, 1);
const vec4 green         = vec4(0.15, 0.41, 0.20, 1.0);
const vec4 yellow        = vec4(0.59, 0.41, 0.00, 1.0);
const vec4 red           = vec4(0.88, 0.00, 0.00, 1.0);

void main() {
  vec3  fragPosition   = vec3(position);
  vec3  unitNormal     = normalize(normal);
  vec3  lightDirection = normalize(lightPosition - fragPosition);
  float lightImpact    = max(dot(unitNormal, lightDirection), 0.0);
  vec3  diffuse        = lightImpact * lightColor;
  float ambientImpact  = 0.1;
  vec3  ambient        = ambientImpact * lightColor;
  
  vec4 objectColor;

  switch (focus) {
    case 1:
      objectColor = green;
      break;
    case 2:
      objectColor = yellow;
      break;
    case 3:
      objectColor = red;
      break;
    default:
      objectColor = texture(textures[texIndex], texCoord);
      break;
  }
  
  color = vec4(ambient + diffuse, 1.0) * objectColor;
}

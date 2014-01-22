#version 330

layout (location = 0) in vec3 position;

uniform mat4 projection_matrix;
uniform mat4 view_matrix;
uniform vec3 offset;

void main(void)
{
  vec3 offset = vec3(-3.0, 1.0, -5.0);
  mat4 translation_matrix = mat4(vec4(1.0, 0.0, 0.0, 0.0),
                                 vec4(0.0, 1.0, 0.0, 0.0),
                                 vec4(0.0, 0.0, 1.0, 0.0),
                                 vec4(offset, 1.0));

  gl_Position = projection_matrix * view_matrix * translation_matrix * vec4(position, 1.0);
}

#version 330

layout (location = 0) in vec3 position;

uniform mat4 projection_matrix;
uniform mat4 view_matrix;
uniform mat4 translation_matrix;

void main(void)
{
  gl_Position = projection_matrix * view_matrix * translation_matrix * vec4(position, 1.0);
}

#version 330

out vec4 out_color;

void main(void)
{
  out_color = mix(vec4(1.0f, 0.5f, 0.5f, 1.0f), vec4(0.5f, 0.5f, 1.0f, 1.0f), (gl_FragCoord.y) / 720.0f);
}

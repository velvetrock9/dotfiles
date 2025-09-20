float warp = 0.25; // simulate curvature of CRT monitor
float scan = 0.50; // darkness between scanlines

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord / iResolution.xy;

    // squared distance from center (for warp)
    vec2 dc = abs(0.5 - uv);
    dc *= dc;

    // warp coords
    uv.x = (uv.x - 0.5) * (1.0 + dc.y * (0.3 * warp)) + 0.5;
    uv.y = (uv.y - 0.5) * (1.0 + dc.x * (0.4 * warp)) + 0.5;

    // clip outside warped bounds to black so curvature is visible
    if (uv.x < 0.0 || uv.x > 1.0 || uv.y < 0.0 || uv.y > 1.0) {
        fragColor = vec4(0.0, 0.0, 0.0, 1.0);
        return;
    }

    // sample source
    vec4 src = texture(iChannel0, uv);

    // strip hue -> luminance
    float lum = dot(src.rgb, vec3(0.25, 0.75, 0.0722));

    // pure orange tint with gentler gamma
    const vec3 ORANGE = vec3(1.4, 0.6, 0.0);
    vec3 amber = pow(lum * ORANGE, vec3(1.2));

    // --- Lift faint UI text ---
    float faintBoost = smoothstep(0.05, 0.25, lum); // detect dark gray range
    amber = mix(amber * 1.6, amber, faintBoost);    // boost dim stuff

    // add a minimum brightness floor so nothing disappears
    amber = max(amber, 0.05 * ORANGE);

    // reduce bg darkening a bit more
    float bgDarken = 0.35; // was 0.4
    float scanMask = abs(sin(fragCoord.y)) * 0.5 * scan;
    float dark = bgDarken * (1.0 - lum) + scanMask;

    vec3 final = mix(amber, vec3(0.0), clamp(dark, 0.0, 1.0));
    fragColor = vec4(final, 1.0);
}


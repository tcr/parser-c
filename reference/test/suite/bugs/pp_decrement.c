void b (int x) {
	if (- (-x) - (-x))
		link_error ();
}
void c (int x) {
	if (+ (+x) - x)
		link_error ();
}

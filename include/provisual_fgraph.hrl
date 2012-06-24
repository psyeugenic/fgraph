-define(fg_th, (0.25)).
-define(fg_damp, (0.75)).
-define(fg_kc, (1000.0)).
-define(fg_stretch, (0.005)).
-define(fg_grav, (9.82)).
-define(fg_sqrt_eps, (0.005)).

%% Ke = 8.854187817e9 [N x M^2 x C^(-2)]
-define(fg_wind, (0.15)).

-record(fg_e, {
	l = 10.0,
	k = 10.0
	}).
	

-record(fg_v, {
	p = {0.0,0.0},
	v = {0.0,0.0},
	q = 5.0,
	m = 1.0,
	type = dynamic,
	state = running,
	resides = undefined,
	selected = false,
	color,
	name
	}).

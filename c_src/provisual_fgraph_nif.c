/*
 * Copyright (c) 2012 Björn-Egil Dahlberg
 *
 * Created: 2012-06-26
 * Author:  Björn-Egil Dahlberg
 *
 */

#include "erl_nif.h"
#include <stdio.h>

/* useful atoms */
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_ok;

static void init(ErlNifEnv *env) {
    /* useful atoms */
    am_error      = enif_make_atom(env, "error");
    am_ok         = enif_make_atom(env, "ok");

}

static ERL_NIF_TERM composition(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    const ERL_NIF_TERM *v1, *v0;
    int arity = 0;
    double v1x, v1y, v1z, v0x, v0y, v0z;
    float dx,dy,dz;
    long i;
    float x2, y, number, r;
    const float threehalfs = 1.5F;

    if (!enif_get_tuple(env, argv[0], &arity, &v1) && arity != 3)
	return enif_make_badarg(env);
    if (!enif_get_tuple(env, argv[1], &arity, &v0) && arity != 3)
	return enif_make_badarg(env);

    if (    !enif_get_double(env, v1[0], &v1x) ||
	    !enif_get_double(env, v1[1], &v1y) ||
	    !enif_get_double(env, v1[2], &v1z) ||

	    !enif_get_double(env, v0[0], &v0x) ||
	    !enif_get_double(env, v0[1], &v0y) ||
	    !enif_get_double(env, v0[2], &v0z)) {
	return enif_make_badarg(env);
    }

    dx = v1x - v0x;
    dy = v1y - v0y;
    dz = v1z - v0z;

    number = dx*dx + dy*dy + dz*dz + 0.0005f;

    x2 = number * 0.5F;
    y  = number;
    i  = * ( long * ) &y;
    i  = 0x5f3759df - ( i >> 1 );
    y  = * ( float * ) &i;
    y  = y * ( threehalfs - ( x2 * y * y ) );

    r = 1/y;

    return enif_make_tuple2(env, enif_make_double(env, r),
	    enif_make_tuple3(env, 
		enif_make_double(env, dx/r),
		enif_make_double(env, dy/r),
		enif_make_double(env, dz/r)
		)
	    );
}


static ErlNifFunc nif_functions[] = {
    {"composition", 2, composition},
};

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    init(env);

    *priv_data = NULL;
    return 0;
}

ERL_NIF_INIT(provisual_fgraph, nif_functions, load, NULL, NULL, NULL)

// The MIT License (MIT)
//
// Copyright (c) 2019 Luigi Pertoldi
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.

import three;

void showaxes(picture pic=currentpicture, triple pos=O) {
    draw(pos -- pos + X, arrow=Arrow3(TeXHead2));
    draw(pos -- pos + Y, arrow=Arrow3(TeXHead2));
    draw(pos -- pos + Z, arrow=Arrow3(TeXHead2));
    label("$x$", pos + X, align=SW);
    label("$y$", pos + Y, align=SE);
    label("$z$", pos + Z, align=N);
}

// define material coloring
material germanium = material(
    diffusepen=gray(0.8),
    emissivepen=black
);

material oxidelayer = material(
    diffusepen=gray(0.8),
    emissivepen=RGB(38,64,115),
    opacity(0.5)
);

real eps_edge_rounding = 0.3;

struct gedet_profile {
    path all;
    path pplus;
    path nplus;
    path passlayer;
}

/*
 * Base struct for detector with common operations
 */

struct gedet {
    gedet_profile profile;

    real passlayer_thickness = 0.1;

    void draw(picture pic=currentpicture, triple pos=O, real angle1=0, real angle2=360) {
        // sanity checks
        if (angle1 >= angle2 || angle2-angle1 > 360) abort("gedetdraw(): invalid input");

        path3 profile3 = path3(this.profile.all, plane=YZplane);
        draw(shift(pos) * surface(profile3 -- cycle, c=O, axis=Z, angle1=angle1, angle2=angle2), surfacepen=germanium);

        if (this.profile.passlayer != nullpath) {
            path3 passlayer3 = path3(this.profile.passlayer, plane=YZplane);
            draw(shift(pos) * surface(passlayer3, c=O, axis=Z, angle1=angle1, angle2=angle2), surfacepen=oxidelayer);
            if (angle2-angle1 != 360) {
                draw(shift(pos) * rotate(angle1, Z) * surface(passlayer3 -- cycle), surfacepen=oxidelayer);
                draw(shift(pos) * rotate(angle2, Z) * surface(passlayer3 -- cycle), surfacepen=oxidelayer);
            }
        }
        // draw faces in cut view
        if (angle2-angle1 != 360) {
            draw(shift(pos) * rotate(angle1, Z) * surface(profile3 -- cycle), surfacepen=germanium);
            draw(shift(pos) * rotate(angle2, Z) * surface(profile3 -- cycle), surfacepen=germanium);
        }
    }
}

/*
 * BEGe geometry
 */

struct BEGe {
    gedet base;
    unravel base;

    real height;
    real radius;
    real groove_depth;
    real groove_inner_r;
    real groove_outer_r;
    real cone_radius;
    real cone_height;
    bool cone_on_top;

    void operator init(real keyword height, real keyword radius, real keyword groove_depth,
                       real keyword groove_inner_r, real keyword groove_outer_r,
                       real keyword cone_radius=0, real keyword cone_height=0,
                       bool keyword cone_on_top=true) {
        this.height = height;
        this.radius = radius;
        this.groove_depth = groove_depth;
        this.groove_inner_r = groove_inner_r;
        this.groove_outer_r = groove_outer_r;
        this.cone_radius = cone_radius;
        this.cone_height = cone_height;
        this.cone_on_top = cone_on_top;

        // apply some edge rounding, to make it look more realistic
        real eps = eps_edge_rounding;

        path p = (0,0)
                 -- (this.groove_inner_r-eps,0){right} .. {up}(this.groove_inner_r,eps)
                 -- (this.groove_inner_r,this.groove_depth-eps){up} .. {right}(this.groove_inner_r+eps,this.groove_depth)
                 -- (this.groove_outer_r-eps,this.groove_depth){right} .. {down}(this.groove_outer_r,this.groove_depth-eps)
                 -- (this.groove_outer_r,eps){down} .. {right}(this.groove_outer_r+eps,0);

        if (this.cone_height != 0 && this.cone_radius != 0) {
            real theta = atan(this.cone_height/this.cone_radius);
            if (this.cone_on_top == true) {
                p = p -- (this.radius-eps,0){right} .. {up}(this.radius,eps)
                      -- (this.radius,this.height-this.cone_height-eps){up}
                      .. (this.radius-eps*cos(theta),this.height-this.cone_height+eps*sin(theta))
                      -- (this.radius-this.cone_radius+eps*cos(theta),this.height-eps*sin(theta))
                      .. {left}(this.radius-this.cone_radius-eps,this.height) -- (0,this.height);
            }
            else {
                p = p -- (this.radius-this.cone_radius-eps,0){right}
                      .. (this.radius-this.cone_radius+eps*cos(theta),eps*sin(theta))
                      -- (this.radius-eps*cos(theta),this.cone_height-eps*sin(theta))
                      .. {up}(this.radius,this.cone_height+eps)
                      -- (this.radius,this.height-eps){up} .. {left}(this.radius-eps,this.height)
                      -- (0,this.height);
            }
        }
        else {
            p = p -- (this.radius-eps,0){right} .. {up}(this.radius,eps)
                  -- (this.radius,this.height-eps){up} .. {left}(this.radius-eps,this.height)
                  -- (0,this.height);
        }
        // center profile (detector) in origin
        this.profile.all = shift(0,-this.height/2) * p;

        // passivation layer
        this.profile.passlayer = (this.groove_inner_r-eps,0){right} .. {up}(this.groove_inner_r,eps)
            -- (this.groove_inner_r,this.groove_depth-eps){up} .. {right}(this.groove_inner_r+eps,this.groove_depth)
            -- (this.groove_outer_r-eps,this.groove_depth){right} .. {down}(this.groove_outer_r,this.groove_depth-eps)
            -- (this.groove_outer_r,eps){down} .. {right}(this.groove_outer_r+eps,0); // and back...
        real delta = this.passlayer_thickness;
        this.profile.passlayer = this.profile.passlayer{left} .. {up}(this.groove_outer_r-delta,eps)
            -- (this.groove_outer_r-delta,this.groove_depth-eps){up} .. {left}(this.groove_outer_r-eps,this.groove_depth-delta)
            -- (this.groove_inner_r+eps,this.groove_depth-delta){left} .. {down}(this.groove_inner_r+delta,this.groove_depth-eps)
            -- (this.groove_inner_r+delta,eps){down} .. {left}(this.groove_inner_r-eps,0);

        // center profile (detector) in origin
        this.profile.passlayer = shift(0,-this.height/2) * this.profile.passlayer;
    }
}

/*
 * Semi-coaxial geometry
 */

struct SemiCoax {
    BEGe bege;
    unravel bege;

    real borehole_depth;
    real borehole_radius;

    void operator init(real keyword height, real keyword radius, real keyword groove_depth,
                       real keyword groove_inner_r, real keyword groove_outer_r,
                       real keyword borehole_radius, real keyword borehole_depth,
                       real keyword cone_radius=0, real keyword cone_height=0,
                       bool keyword cone_on_top=true) {
        bege.operator init(height=height, radius=radius, groove_depth=groove_depth,
                           groove_inner_r=groove_inner_r, groove_outer_r=groove_outer_r,
                           cone_radius=cone_radius, cone_height=cone_height, cone_on_top=cone_on_top);
        this.borehole_radius = borehole_radius;
        this.borehole_depth = borehole_depth;

        // apply some edge rounding, to make it look more realistic
        real eps = eps_edge_rounding;

        path p = (0,this.borehole_depth)
                 -- (this.borehole_radius-eps,this.borehole_depth){right}
                 .. {down}(this.borehole_radius,this.borehole_depth-eps)
                 -- (this.borehole_radius,0+eps){down} .. {right}(this.borehole_radius+eps,0)
                 -- (this.groove_inner_r-eps,0){right} .. {up}(this.groove_inner_r,eps)
                 -- (this.groove_inner_r,this.groove_depth-eps){up} .. {right}(this.groove_inner_r+eps,this.groove_depth)
                 -- (this.groove_outer_r-eps,this.groove_depth){right} .. {down}(this.groove_outer_r,this.groove_depth-eps)
                 -- (this.groove_outer_r,eps){down} .. {right}(this.groove_outer_r+eps,0);

        if (this.cone_height != 0 && this.cone_radius != 0) {
            real theta = atan(this.cone_height/this.cone_radius);
            if (this.cone_on_top == true) {
                p = p -- (this.radius-eps,0){right} .. {up}(this.radius,eps)
                      -- (this.radius,this.height-this.cone_height-eps){up}
                      .. (this.radius-eps*cos(theta),this.height-this.cone_height+eps*sin(theta))
                      -- (this.radius-this.cone_radius+eps*cos(theta),this.height-eps*sin(theta))
                      .. {left}(this.radius-this.cone_radius-eps,this.height) -- (0,this.height);
            }
            else {
                p = p -- (this.radius-this.cone_radius-eps,0){right}
                      .. (this.radius-this.cone_radius+eps*cos(theta),eps*sin(theta))
                      -- (this.radius-eps*cos(theta),this.cone_height-eps*sin(theta))
                      .. {up}(this.radius,this.cone_height+eps)
                      -- (this.radius,this.height-eps){up} .. {left}(this.radius-eps,this.height)
                      -- (0,this.height);
            }
        }
        else {
            p = p -- (this.radius-eps,0){right} .. {up}(this.radius,eps)
                  -- (this.radius,this.height-eps){up} .. {left}(this.radius-eps,this.height)
                  -- (0,this.height);
        }
        // center profile (detector) in origin
        this.profile.all = shift(0,-this.height/2) * p;
    }
}

/*
 * Inverted-coaxial geometry
 */

struct InvCoax {
    BEGe bege;
    unravel bege;

    real borehole_depth;
    real borehole_radius;

    void operator init(real keyword height, real keyword radius, real keyword groove_depth,
                       real keyword groove_inner_r, real keyword groove_outer_r,
                       real keyword borehole_radius, real keyword borehole_depth,
                       real keyword cone_radius=0, real keyword cone_height=0,
                       bool keyword cone_on_top=true) {
        bege.operator init(height=height, radius=radius, groove_depth=groove_depth,
                           groove_inner_r=groove_inner_r, groove_outer_r=groove_outer_r,
                           cone_radius=cone_radius, cone_height=cone_height, cone_on_top=cone_on_top);
        this.borehole_radius = borehole_radius;
        this.borehole_depth = borehole_depth;

        // apply some edge rounding, to make it look more realistic
        real eps = eps_edge_rounding;

        path p = (0,0)
                 -- (this.groove_inner_r-eps,0){right} .. {up}(this.groove_inner_r,eps)
                 -- (this.groove_inner_r,this.groove_depth-eps){up} .. {right}(this.groove_inner_r+eps,this.groove_depth)
                 -- (this.groove_outer_r-eps,this.groove_depth){right} .. {down}(this.groove_outer_r,this.groove_depth-eps)
                 -- (this.groove_outer_r,eps){down} .. {right}(this.groove_outer_r+eps,0);

        if (this.cone_height != 0 && this.cone_radius != 0) {
            real theta = atan(this.cone_height/this.cone_radius);
            if (this.cone_on_top == true) {
                p = p -- (this.radius-eps,0){right} .. {up}(this.radius,eps)
                      -- (this.radius,this.height-this.cone_height-eps){up}
                      .. (this.radius-eps*cos(theta),this.height-this.cone_height+eps*sin(theta))
                      -- (this.radius-this.cone_radius+eps*cos(theta),this.height-eps*sin(theta))
                      .. {left}(this.radius-this.cone_radius-eps,this.height);
            }
            else {
                p = p -- (this.radius-this.cone_radius-eps,0){right}
                      .. (this.radius-this.cone_radius+eps*cos(theta),eps*sin(theta))
                      -- (this.radius-eps*cos(theta),this.cone_height-eps*sin(theta))
                      .. {up}(this.radius,this.cone_height+eps)
                      -- (this.radius,this.height-eps){up} .. {left}(this.radius-eps,this.height);
            }
        }
        else {
            p = p -- (this.radius-eps,0){right} .. {up}(this.radius,eps)
                  -- (this.radius,this.height-eps){up} .. {left}(this.radius-eps,this.height);
        }
        // add borehole on top
        p = p -- (this.borehole_radius+eps,this.height){left}
              .. {down}(this.borehole_radius,this.height-eps)
              -- (this.borehole_radius,this.height-this.borehole_depth+eps){down}
              .. {left}(this.borehole_radius-eps,this.height-this.borehole_depth)
              -- (0,this.height-this.borehole_depth);

        // center profile (detector) in origin
        this.profile.all = shift(0,-this.height/2) * p;
    }
}

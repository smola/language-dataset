@** The Point. The point is the atomic value used inside of libline. 

@<Top@>+= @<The Point@>

@* The |ll_point| struct declaration.
@ A libline point can be best thought of as a line chunk going from
point A to point B over a given duration in seconds.

@<The Point@>+=
struct ll_point {@/
    ll_flt A;
    ll_flt dur;
    ll_flt *B;

@ The line is built around a linked list data structure, so the struct has
a reference to the next entry in the list. 

@<The Point@>+=
    ll_point *next;

@ Points have various styles of interpolation, and with that comes custom
user data for the allocator, and memory allocation.

@<The Point@>+=
    ll_cb_malloc malloc;
    ll_cb_free free;
    void *ud;

@ Custom data needs to be freed in a general kind of way. This is another 
free callback called destroy. This should be called before free.

@<The Point@>+=
    void *data;
    ll_cb_free destroy;

@ A step function computes a line segment local to the point. By default,
this is set to return point A>
@<The Point@>+=
    ll_cb_step step;


@/};

@* Point Initialization.
@ The size of the point struct is implemented as a function.

@<The Point@>+=
size_t ll_point_size(void)
{
    return sizeof(ll_point);
}

@ Initialization. Add some words here.

@<The Point@>+=
@<Default Step Function@>
void ll_point_init(ll_point *pt)
{
    pt->A = 1.0; /* A reasonable default value */
    pt->dur = 1.0; /* A one-second duration by default */
    pt->B = &pt->A; /* Point B points to point A by default */
    pt->ud = NULL;
    pt->free = ll_free;
    pt->malloc = ll_malloc;
    pt->data = NULL;
    pt->destroy = ll_free_nothing;
    pt->step = step;
}

@* Point Setters and Getters. The following describes setter and getter
functions needed for the |ll_point| opaque pointer type.

@ Set the initial "A" value. 
@<The Point@>+=
void ll_point_value(ll_point *pt, ll_flt val)
{
    pt->A = val;
}


@ This sets the point of the "B" value. Note that this is a pointer value.

@<The Point@>+=
void ll_point_set_next_value(ll_point *pt, ll_flt *val)
{
    pt->B = val;
}

@ Set the point duration. 
@<The Point@>+=
void ll_point_dur(ll_point *pt, ll_flt dur)
{
    pt->dur = dur;
}

ll_flt ll_point_get_dur(ll_point *pt)
{
    return pt->dur;
}

@ The following function is used to set the next entry in the linked list.
@<The Point@>+=
void ll_point_set_next_point(ll_point *pt, ll_point *next)
{
    pt->next = next;
}

@ The following function is used to retrive the next entry in the linked list.
@<The Point@>+=
ll_point * ll_point_get_next_point(ll_point *pt)
{
    return pt->next;
}

@ In order to set a B value, there needs to be a way to get the memory address
of another points A value. This function returns the memory address of a points
A value.
@<The Point@>+=
ll_flt * ll_point_get_value(ll_point *pt)
{
    return &pt->A;
}

@ These functions return the A and B values in the point struct, and 
are particularly useful for interpolation functions.
@<The Point@>+=
ll_flt ll_point_A(ll_point *pt)
{
    return pt->A;
}

ll_flt ll_point_B(ll_point *pt)
{
    return *pt->B;
}

@* Point Memory Handling.

@ Various interpolation styles will require the ability to allocate memory.
For this reason, the memory allocation functions must be exposed. 

@<The Point@>+=
void *ll_point_malloc(ll_point *pt, size_t size)
{
    return pt->malloc(pt->ud, size);
}
void ll_point_free(ll_point *pt, void *ptr)
{
    pt->free(pt->ud, ptr);
}

@ Data allocated by the interpolator is {\it destroyed} using the internal
free function. 
@<The Point@>+=
void ll_point_destroy(ll_point *pt)
{
    pt->destroy(pt, pt->data);
}

@* Point Step Function. Every point has a "step" function, which computes
the current points value at that moment in time. 

@ The default step function simply returns point A.
@<Default Step Function@>=
static ll_flt step(ll_point *pt, void *ud, UINT pos, UINT dur)
{
    return ll_point_A(pt);
}

@ These functions set the internal variables for the step function, step
function data, and the destroy function, respectively.

@<Default Step Function@>=
void ll_point_data(ll_point *pt, void *data)
{
    pt->data = data;
}

void ll_point_cb_step(ll_point *pt, ll_cb_step stp)
{
   pt->step = stp;
}

void ll_point_cb_destroy(ll_point *pt, ll_cb_free destroy)
{
    pt->destroy = destroy;
}

@ This calls the step function inside of the point.
@<The Point@>+=
ll_flt ll_point_step(ll_point *pt, UINT pos, UINT dur)
{
    return pt->step(pt, pt->data, pos, dur);
}

@ The function |ll_point_mem_callback| sets the memory allocation callbacks.
This function may be called implicitely when setting memory allocation functions
from higher abstractions.
@<The Point@>+=
void ll_point_mem_callback(ll_point *pt, ll_cb_malloc m, ll_cb_free f)
{
    pt->malloc = m;
    pt->free = f;
}

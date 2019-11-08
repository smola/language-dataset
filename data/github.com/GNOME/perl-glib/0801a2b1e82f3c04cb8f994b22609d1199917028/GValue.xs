/*
 * Copyright (C) 2003-2009 by the gtk2-perl team (see the file AUTHORS for the
 * full list)
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * $Id$
 */

=head2 GValue

GValue is GLib's generic value container, and it is because of GValue that the
run time type handling of GObject parameters and GClosure marshaling can
function, and most usages of these functions will be from those two points.

Client code will run into uses for gperl_sv_from_value() and
gperl_value_from_sv() when trying to convert lists of parameters into GValue
arrays and the like.

=over

=cut

#include "gperl.h"


/****************************************************************************
 * GValue handling
 *
 * we have code here to handle the fundamental types listed in the API
 * reference, plus the G_TYPE_ENUM and G_TYPE_FLAGS fundamentals.  new
 * fundamentals created by g_type_fundamental_next() are handled by the
 * GPerlValueWrapperClass machinery.
 */

=item gboolean gperl_value_from_sv (GValue * value, SV * sv)

set a I<value> from a whatever is in I<sv>.  I<value> must be initialized 
so the code knows what kind of value to coerce out of I<sv>.

Return value is always TRUE; if the code knows how to perform the conversion,
it croaks.  (The return value is for backward compatibility.) In reality,
this really ought to always succeed; a failed conversion should be considered
a bug or unimplemented code!

=cut
gboolean
gperl_value_from_sv (GValue * value,
		     SV * sv)
{
	GType type;
	if (!gperl_sv_is_defined (sv))
		return TRUE; /* use the GValue type's default */
	type = G_TYPE_FUNDAMENTAL (G_VALUE_TYPE (value));
	/*printf ("TYPE: %d, S: %s\n", type, SvPV_nolen(sv));*/
	switch (type) {
    		case G_TYPE_INTERFACE:
			/* pygtk mentions something about only handling 
			   GInterfaces with a GObject prerequisite.  i'm
			   just blindly treating them as objects until
			   this breaks and i understand what they mean. */
    			g_value_set_object(value, gperl_get_object(sv));
			break;
		case G_TYPE_CHAR:
		{
			gchar *tmp = SvGChar (sv);
#if GLIB_CHECK_VERSION(2, 32, 0)
			g_value_set_schar (value, (gint8)(tmp ? tmp[0] : 0));
#else
			g_value_set_char (value, (gchar)(tmp ? tmp[0] : 0));
#endif
			break;
		}
		case G_TYPE_UCHAR:
		{
			char *tmp = SvPV_nolen (sv);
			g_value_set_uchar (value, (guchar)(tmp ? tmp[0] : 0));
			break;
		}
		case G_TYPE_BOOLEAN:
			/* undef is also false. */
			g_value_set_boolean (value, SvTRUE (sv));
			break;
		case G_TYPE_INT:
			g_value_set_int(value, SvIV(sv));
			break;
		case G_TYPE_UINT:
			g_value_set_uint(value, SvIV(sv));
			break;
		case G_TYPE_LONG:
			g_value_set_long(value, SvIV(sv));
			break;
		case G_TYPE_ULONG:
			g_value_set_ulong(value, SvIV(sv));
			break;
		case G_TYPE_INT64:
			g_value_set_int64(value, SvGInt64(sv));
			break;
		case G_TYPE_UINT64:
			g_value_set_uint64(value, SvGUInt64(sv));
			break;
		case G_TYPE_FLOAT:
			g_value_set_float(value, (gfloat)SvNV(sv));
			break;
		case G_TYPE_DOUBLE:
			g_value_set_double(value, SvNV(sv));
			break;
		case G_TYPE_STRING:
			g_value_set_string(value, SvGChar(sv));
			break;
		case G_TYPE_POINTER:
#if GLIB_CHECK_VERSION(2, 10, 0)
			/* The fundamental type for G_TYPE_GTYPE is
			 * G_TYPE_POINTER, so we have to treat this
			 * specially. */
			if (G_VALUE_TYPE (value) == G_TYPE_GTYPE) {
				g_value_set_gtype (value, gperl_type_from_package (SvGChar (sv)));
				break;
			}
#endif
			g_value_set_pointer (value,
			                     INT2PTR (gpointer, SvIV (sv)));
			break;
		case G_TYPE_BOXED:
			/* SVs need special treatment! */
			if (G_VALUE_HOLDS (value, GPERL_TYPE_SV)) {
				g_value_set_boxed (value,
				                   gperl_sv_is_defined (sv)
				                   ? sv : NULL);
			} else {
				g_value_set_static_boxed (
					value,
					gperl_get_boxed_check (
						sv, G_VALUE_TYPE(value)));
			}
			break;
		case G_TYPE_PARAM:
			g_value_set_param(value, SvGParamSpec (sv));
			break;
		case G_TYPE_OBJECT:
			g_value_set_object(value, gperl_get_object_check (sv, G_VALUE_TYPE(value)));
			break;

		case G_TYPE_ENUM:
			g_value_set_enum(value, gperl_convert_enum(G_VALUE_TYPE(value), sv));
			break;
		case G_TYPE_FLAGS:
			g_value_set_flags(value, gperl_convert_flags(G_VALUE_TYPE(value), sv));
			break;

		default: {
			GPerlValueWrapperClass *wrapper_class;

			wrapper_class = gperl_fundamental_wrapper_class_from_type (type);
			if (wrapper_class && wrapper_class->unwrap) {
				wrapper_class->unwrap (value, sv);
				break;
			}

			croak ("[gperl_value_from_sv] FIXME: unhandled type - %lu (%s fundamental for %s)\n",
			       type, g_type_name (type), G_VALUE_TYPE_NAME (value));
			return FALSE;
		}
	}
	return TRUE;
}


/*
 * =item SV * _gperl_sv_from_value_internal (const GValue * value, gboolean copy_boxed)
 *
 * Coerce whatever is in I<value> into a perl scalar and return it.
 * If I<copy_boxed> is true, boxed values will be copied.  Values of type
 * GPERL_TYPE_SV are always copied (since that is merely a ref).
 *
 * Croaks if the code doesn't know how to perform the conversion.
 *
 * Might end up calling other Perl code.  So if you use this function in XS
 * code for a generic GType, make sure the stack pointer is set up correctly
 * before the call, and restore it after the call.
 *
 * =cut
 */
SV *
_gperl_sv_from_value_internal (const GValue * value,
                               gboolean copy_boxed)
{
	GType type = G_TYPE_FUNDAMENTAL (G_VALUE_TYPE (value));
	switch (type) {
    		case G_TYPE_INTERFACE:
			/* pygtk mentions something about only handling 
			   GInterfaces with a GObject prerequisite.  i'm
			   just blindly treating them as objects until
			   this breaks and i understand what they mean. */
			return gperl_new_object (g_value_get_object (value), FALSE);
		case G_TYPE_CHAR:
#if GLIB_CHECK_VERSION(2, 32, 0)
			return newSViv (g_value_get_schar (value));
#else
			return newSViv (g_value_get_char (value));
#endif

		case G_TYPE_UCHAR:
			return newSVuv (g_value_get_uchar (value));

		case G_TYPE_BOOLEAN:
			return newSViv(g_value_get_boolean(value));

		case G_TYPE_INT:
			return newSViv(g_value_get_int(value));

		case G_TYPE_UINT:
			return newSVuv(g_value_get_uint(value));

		case G_TYPE_LONG:
			return newSViv(g_value_get_long(value));

		case G_TYPE_ULONG:
			return newSVuv(g_value_get_ulong(value));

		case G_TYPE_INT64:
			return newSVGInt64(g_value_get_int64(value));

		case G_TYPE_UINT64:
			return newSVGUInt64(g_value_get_uint64(value));

		case G_TYPE_FLOAT:
			return newSVnv(g_value_get_float(value));

		case G_TYPE_DOUBLE:
			return newSVnv(g_value_get_double(value));

		case G_TYPE_STRING:
			return newSVGChar (g_value_get_string (value));

		case G_TYPE_POINTER:
#if GLIB_CHECK_VERSION(2, 10, 0)
			/* The fundamental type for G_TYPE_GTYPE is
			 * G_TYPE_POINTER, so we have to treat this
			 * specially. */
			if (G_VALUE_TYPE (value) == G_TYPE_GTYPE) {
				GType gtype = g_value_get_gtype (value);
				return newSVGChar (
				  gtype == G_TYPE_NONE
				         ? NULL
				         : gperl_package_from_type (gtype));
			}
#endif
			return newSViv (PTR2IV (g_value_get_pointer (value)));

		case G_TYPE_BOXED:
			/* special case for SVs, which are stored directly
			 * rather than inside blessed wrappers. */
			if (G_VALUE_HOLDS (value, GPERL_TYPE_SV)) {
				SV * sv = g_value_get_boxed (value);
				return sv ? g_value_dup_boxed (value)
				          : &PL_sv_undef;
			}

                        if (copy_boxed)
                                return gperl_new_boxed_copy
                                                (g_value_get_boxed (value),
                                                 G_VALUE_TYPE (value));
                        else
                                return gperl_new_boxed
                                                (g_value_get_boxed (value),
                                                 G_VALUE_TYPE (value),
                                                 FALSE);

		case G_TYPE_PARAM:
			/* can have NULL here fetching object properties of
			 * type G_TYPE_PARAM with no value set yet, or from
			 * ->get_default_value of such a
			 * property. newSVGParamSpec handles NULL by returning
			 * undef. */
			return newSVGParamSpec (g_value_get_param (value));

		case G_TYPE_OBJECT:
			return gperl_new_object (g_value_get_object (value), FALSE);

		case G_TYPE_ENUM:
			return gperl_convert_back_enum (G_VALUE_TYPE (value),
							g_value_get_enum (value));

		case G_TYPE_FLAGS:
			return gperl_convert_back_flags (G_VALUE_TYPE (value),
							 g_value_get_flags (value));

		default: {
			GPerlValueWrapperClass *wrapper_class;

			wrapper_class = gperl_fundamental_wrapper_class_from_type (type);
			if (wrapper_class && wrapper_class->wrap)
				return wrapper_class->wrap (value);

			croak ("[gperl_sv_from_value] FIXME: unhandled type - %lu (%s fundamental for %s)\n",
			       type, g_type_name (type), G_VALUE_TYPE_NAME (value));
		}
	}

	return NULL;
}

=item SV * gperl_sv_from_value (const GValue * value)

Coerce whatever is in I<value> into a perl scalar and return it.

Croaks if the code doesn't know how to perform the conversion.

Might end up calling other Perl code.  So if you use this function in XS code
for a generic GType, make sure the stack pointer is set up correctly before the
call, and restore it after the call.

=cut
SV *
gperl_sv_from_value (const GValue * value)
{
	return _gperl_sv_from_value_internal (value, FALSE);
}

=back

=cut

/* apparently this line is required by ExtUtils::ParseXS, but not by xsubpp. */
MODULE = Glib::Value	PACKAGE = Glib::Value	PREFIX = g_value_

PROTOTYPES: ENABLE

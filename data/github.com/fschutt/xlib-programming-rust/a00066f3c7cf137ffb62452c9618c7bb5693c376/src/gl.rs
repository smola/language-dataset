/* Extensions */
const GL_OES_SC_VERSION_1_0             1
const GL_EXT_PALETTED_TEXTURE           1

/* ClearBufferMask */
const GL_DEPTH_BUFFER_BIT               0x00000100
const GL_STENCIL_BUFFER_BIT             0x00000400
const GL_COLOR_BUFFER_BIT               0x00004000

/* Boolean */
const GL_FALSE                          0
const GL_TRUE                           1

/* BeginMode */
const GL_POINTS                         0x0000
const GL_LINES                          0x0001
const GL_LINE_LOOP                      0x0002
const GL_LINE_STRIP                     0x0003
const GL_TRIANGLES                      0x0004
const GL_TRIANGLE_STRIP                 0x0005
const GL_TRIANGLE_FAN                   0x0006

/* AlphaFunction */
/* const GL_LEQUAL                         0x0203 */
/* const GL_ALWAYS                         0x0207 */

/* BlendingFactorDest */
const GL_ZERO                           0
const GL_ONE                            1
const GL_ONE_MINUS_SRC_ALPHA            0x0303

/* BlendingFactorSrc */
/* const GL_ONE                            1 */
const GL_SRC_ALPHA_SATURATE             0x0308
const GL_SRC_ALPHA                      0x0302

/* ColorMaterialFace */
/* const GL_FRONT_AND_BACK                 0x0408 */

/* ColorMaterialParameter */
/* const GL_AMBIENT_AND_DIFFUSE            0x1602 */

/* ColorPointerType */
/* const GL_FLOAT                          0x1406 */
/* const GL_UNSIGNED_BYTE                  0x1401 */

/* CullFaceMode */
const GL_FRONT                          0x0404
const GL_BACK                           0x0405
const GL_FRONT_AND_BACK                 0x0408

/* DepthFunction */
/* const GL_LESS                           0x0201 */
/* const GL_LEQUAL                         0x0203 */
/* const GL_ALWAYS                         0x0207 */

/* EnableCap */
const GL_LIGHTING                       0x0B50
const GL_TEXTURE_2D                     0x0DE1
const GL_CULL_FACE                      0x0B44
const GL_ALPHA_TEST                     0x0BC0
const GL_BLEND                          0x0BE2
const GL_STENCIL_TEST                   0x0B90
const GL_DEPTH_TEST                     0x0B71
const GL_LIGHT0                         0x4000
const GL_LIGHT1                         0x4001
const GL_POINT_SMOOTH                   0x0B10
const GL_LINE_STIPPLE                   0x0B24
const GL_LINE_SMOOTH                    0x0B20
const GL_SCISSOR_TEST                   0x0C11
const GL_COLOR_MATERIAL                 0x0B57
const GL_NORMALIZE                      0x0BA1
const GL_RESCALE_NORMAL                 0x803A
const GL_POLYGON_OFFSET_FILL            0x8037
const GL_POLYGON_STIPPLE                0x0B42
const GL_VERTEX_ARRAY                   0x8074
const GL_NORMAL_ARRAY                   0x8075
const GL_COLOR_ARRAY                    0x8076
const GL_TEXTURE_COORD_ARRAY            0x8078

/* ErrorCode */
const GL_NO_ERROR                       0
const GL_INVALID_ENUM                   0x0500
const GL_INVALID_VALUE                  0x0501
const GL_INVALID_OPERATION              0x0502
const GL_STACK_OVERFLOW                 0x0503
const GL_STACK_UNDERFLOW                0x0504
const GL_OUT_OF_MEMORY                  0x0505

/* FogMode */

/* FogParameter */

/* FrontFaceDirection */
const GL_CW                             0x0900
const GL_CCW                            0x0901

/* GetBooleanv */
const GL_DEPTH_WRITEMASK                0x0B72
const GL_COLOR_WRITEMASK                0x0C23

/* GetFloatv */
const GL_CURRENT_COLOR                  0x0B00
const GL_CURRENT_NORMAL                 0x0B02
const GL_CURRENT_TEXTURE_COORDS         0x0B03
const GL_CURRENT_RASTER_COLOR           0x0B04
const GL_CURRENT_RASTER_TEXTURE_COORDS  0x0B06
const GL_POINT_SIZE                     0x0B11
const GL_SMOOTH_POINT_SIZE_RANGE        0x0B12
const GL_SMOOTH_POINT_SIZE_GRANULARITY  0x0B13
const GL_LINE_WIDTH                     0x0B21
const GL_SMOOTH_LINE_WIDTH_RANGE        0x0B22
const GL_SMOOTH_LINE_WIDTH_GRANULARITY  0x0B23
const GL_LIGHT_MODEL_AMBIENT            0x0B53
const GL_DEPTH_RANGE                    0x0B70
const GL_DEPTH_CLEAR_VALUE              0x0B73
const GL_ALPHA_TEST_REF                 0x0BC2
const GL_COLOR_CLEAR_VALUE              0x0C22
const GL_POLYGON_OFFSET_UNITS           0x2A00
const GL_POLYGON_OFFSET_FACTOR          0x8038
const GL_ALIASED_POINT_SIZE_RANGE       0x846D
const GL_ALIASED_LINE_WIDTH_RANGE       0x846E

/* GetIntegerv */
const GL_MATRIX_MODE                    0x0BA0
const GL_VIEWPORT                       0x0BA2
const GL_MODELVIEW_STACK_DEPTH          0x0BA3
const GL_PROJECTION_STACK_DEPTH         0x0BA4
const GL_MODELVIEW_MATRIX               0x0BA6
const GL_PROJECTION_MATRIX              0x0BA7
const GL_LINE_STIPPLE_PATTERN           0x0B25
const GL_LINE_STIPPLE_REPEAT            0x0B26
const GL_MAX_LIST_NESTING               0x0B31
const GL_LIST_BASE                      0x0B32
const GL_CULL_FACE_MODE                 0x0B45
const GL_FRONT_FACE                     0x0B46
const GL_DEPTH_FUNC                     0x0B74
const GL_STENCIL_CLEAR_VALUE            0x0B91
const GL_STENCIL_FUNC                   0x0B92
const GL_STENCIL_VALUE_MASK             0x0B93
const GL_STENCIL_FAIL                   0x0B94
const GL_STENCIL_PASS_DEPTH_FAIL        0x0B95
const GL_STENCIL_PASS_DEPTH_PASS        0x0B96
const GL_STENCIL_REF                    0x0B97
const GL_STENCIL_WRITEMASK              0x0B98
const GL_ALPHA_TEST_FUNC                0x0BC1
const GL_BLEND_DST                      0x0BE0
const GL_BLEND_SRC                      0x0BE1
const GL_SCISSOR_BOX                    0x0C10
const GL_PERSPECTIVE_CORRECTION_HINT    0x0C50
const GL_POINT_SMOOTH_HINT              0x0C51
const GL_LINE_SMOOTH_HINT               0x0C52
const GL_POLYGON_SMOOTH_HINT            0x0C53
const GL_UNPACK_ALIGNMENT               0x0CF5
const GL_PACK_ALIGNMENT                 0x0D05
const GL_MAX_LIGHTS                     0x0D31
const GL_MAX_TEXTURE_SIZE               0x0D33
const GL_MAX_MODELVIEW_STACK_DEPTH      0x0D36
const GL_MAX_PROJECTION_STACK_DEPTH     0x0D38
const GL_MAX_VIEWPORT_DIMS              0x0D3A
const GL_SUBPIXEL_BITS                  0x0D50
const GL_RED_BITS                       0x0D52
const GL_GREEN_BITS                     0x0D53
const GL_BLUE_BITS                      0x0D54
const GL_ALPHA_BITS                     0x0D55
const GL_DEPTH_BITS                     0x0D56
const GL_STENCIL_BITS                   0x0D57
const GL_VERTEX_ARRAY_SIZE              0x807A 
const GL_VERTEX_ARRAY_TYPE              0x807B 
const GL_VERTEX_ARRAY_STRIDE            0x807C 
const GL_NORMAL_ARRAY_TYPE              0x807E 
const GL_NORMAL_ARRAY_STRIDE            0x807F 
const GL_COLOR_ARRAY_SIZE               0x8081 
const GL_COLOR_ARRAY_TYPE               0x8082 
const GL_COLOR_ARRAY_STRIDE             0x8083 
const GL_TEXTURE_COORD_ARRAY_SIZE       0x8088 
const GL_TEXTURE_COORD_ARRAY_TYPE       0x8089 
const GL_TEXTURE_COORD_ARRAY_STRIDE     0x808A 
const GL_SHADE_MODEL                    0x0B54
const GL_TEXTURE_BINDING_2D             0x8069
const GL_MAX_ELEMENTS_VERTICES          0x80E8
const GL_MAX_ELEMENTS_INDICES           0x80E9
const GL_ACTIVE_TEXTURE                 0x84E0
const GL_CLIENT_ACTIVE_TEXTURE          0x84E1
const GL_MAX_TEXTURE_UNITS              0x84E2

/* GetMaterialfv */
/* const GL_AMBIENT                        0x1200 */
/* const GL_DIFFUSE                        0x1201 */
/* const GL_SPECULAR                       0x1202 */
/* const GL_EMISSION                       0x1600 */
/* const GL_SHININESS                      0x1601 */

/* GetLightfv */
/* const GL_AMBIENT                        0x1200 */
/* const GL_DIFFUSE                        0x1201 */
/* const GL_SPECULAR                       0x1202 */
/* const GL_POSITION                       0x1203 */

/* GetPointerv */
const GL_VERTEX_ARRAY_POINTER              0x808E
const GL_NORMAL_ARRAY_POINTER              0x808F
const GL_COLOR_ARRAY_POINTER               0x8090
const GL_TEXTURE_COORD_ARRAY_POINTER       0x8092

/* GetTexParameter */
/* const GL_TEXTURE_MAG_FILTER             0x2800 */
/* const GL_TEXTURE_MIN_FILTER             0x2801 */
/* const GL_TEXTURE_WRAP_S                 0x2802 */
/* const GL_TEXTURE_WRAP_T                 0x2803 */

/* GetTexEnvfv */
/* const GL_TEXTURE_ENV_MODE               0x2200 */
/* const GL_TEXTURE_ENV_COLOR              0x2201 */

/* HintMode */
const GL_DONT_CARE                      0x1100
const GL_FASTEST                        0x1101
const GL_NICEST                         0x1102

/* HintTarget */
const GL_PERSPECTIVE_CORRECTION_HINT    0x0C50
const GL_POINT_SMOOTH_HINT              0x0C51
const GL_LINE_SMOOTH_HINT               0x0C52

/* IsEnabled */
/* const GL_LIGHTING                       0x0B50 */
/* const GL_TEXTURE_2D                     0x0DE1 */
/* const GL_CULL_FACE                      0x0B44 */
/* const GL_ALPHA_TEST                     0x0BC0 */
/* const GL_BLEND                          0x0BE2 */
/* const GL_STENCIL_TEST                   0x0B90 */
/* const GL_DEPTH_TEST                     0x0B71 */
/* const GL_LIGHT0                         0x4000 */
/* const GL_LIGHT1                         0x4001 */
/* const GL_POINT_SMOOTH                   0x0B10 */
/* const GL_LINE_STIPPLE                   0x0B24 */
/* const GL_LINE_SMOOTH                    0x0B20 */
/* const GL_SCISSOR_TEST                   0x0C11 */
/* const GL_COLOR_MATERIAL                 0x0B57 */
/* const GL_NORMALIZE                      0x0BA1 */
/* const GL_RESCALE_NORMAL                 0x803A */
/* const GL_POLYGON_OFFSET_FILL            0x8037 */
/* const GL_POLYGON_STIPPLE                0x0B42 */
/* const GL_VERTEX_ARRAY                   0x8074 */
/* const GL_NORMAL_ARRAY                   0x8075 */
/* const GL_COLOR_ARRAY                    0x8076 */
/* const GL_TEXTURE_COORD_ARRAY            0x8078 */

/* LightModelParameter */
const GL_LIGHT_MODEL_AMBIENT            0x0B53

/* LightParameter */
const GL_AMBIENT                        0x1200
const GL_DIFFUSE                        0x1201
const GL_SPECULAR                       0x1202
const GL_POSITION                       0x1203

/* DataType */
const GL_BYTE                           0x1400
const GL_UNSIGNED_BYTE                  0x1401
const GL_FLOAT                          0x1406

/* LogicOp */

/* MaterialFace */
/* const GL_FRONT_AND_BACK                 0x0408 */

/* MaterialParameter */
/* const GL_AMBIENT                        0x1200 */
/* const GL_DIFFUSE                        0x1201 */
/* const GL_SPECULAR                       0x1202 */
const GL_EMISSION                       0x1600
const GL_SHININESS                      0x1601
const GL_AMBIENT_AND_DIFFUSE            0x1602

/* MatrixMode */
const GL_MODELVIEW                      0x1700
const GL_PROJECTION                     0x1701

/* NormalPointerType */
/* const GL_FLOAT                          0x1406 */

/* PixelFormat */
const GL_ALPHA                          0x1906
const GL_RGB                            0x1907
const GL_RGBA                           0x1908
const GL_LUMINANCE                      0x1909
const GL_LUMINANCE_ALPHA                0x190A
const GL_COLOR_INDEX                    0x1900

/* PixelStoreParameter */
const GL_UNPACK_ALIGNMENT               0x0CF5
const GL_PACK_ALIGNMENT                 0x0D05

/* PixelType */
/* const GL_UNSIGNED_BYTE                  0x1401 */

/* ReadPixels */
const GL_COLOR                          0x1800

/* ShadingModel */
const GL_FLAT                           0x1D00
const GL_SMOOTH                         0x1D01

/* StencilFunction */
const GL_NEVER                          0x0200 
const GL_LESS                           0x0201
const GL_EQUAL                          0x0202
const GL_LEQUAL                         0x0203
const GL_GREATER                        0x0204
const GL_NOTEQUAL                       0x0205
const GL_GEQUAL                         0x0206
const GL_ALWAYS                         0x0207

/* StencilOp */
/* const GL_ZERO                           0 */
const GL_KEEP                           0x1E00
const GL_REPLACE                        0x1E01
const GL_INCR                           0x1E02
const GL_DECR                           0x1E03
const GL_INVERT                         0x150A

/* StringName */
const GL_VENDOR                         0x1F00
const GL_RENDERER                       0x1F01
const GL_VERSION                        0x1F02
const GL_EXTENSIONS                     0x1F03

/* TexCoordPointerType */
/* const GL_FLOAT                          0x1406 */

/* TextureEnvMode */
const GL_MODULATE                       0x2100
const GL_DECAL                          0x2101
/* const GL_BLEND                          0x0BE2 */
const GL_ADD                            0x0104
/* const GL_REPLACE                        0x1E01 */

/* TextureEnvParameter */
const GL_TEXTURE_ENV_MODE               0x2200
const GL_TEXTURE_ENV_COLOR              0x2201

/* TextureEnvTarget */
const GL_TEXTURE_ENV                    0x2300

/* TextureMagFilter */
const GL_NEAREST                        0x2600
const GL_LINEAR                         0x2601

/* TextureMinFilter */
/* const GL_NEAREST                        0x2600 */
/* const GL_LINEAR                         0x2601 */
const GL_NEAREST_MIPMAP_NEAREST         0x2700
const GL_LINEAR_MIPMAP_NEAREST          0x2701
const GL_NEAREST_MIPMAP_LINEAR          0x2702
const GL_LINEAR_MIPMAP_LINEAR           0x2703

/* TextureParameterName */
const GL_TEXTURE_MAG_FILTER             0x2800
const GL_TEXTURE_MIN_FILTER             0x2801
const GL_TEXTURE_WRAP_S                 0x2802
const GL_TEXTURE_WRAP_T                 0x2803

/* TextureTarget */
/* const GL_TEXTURE_2D                     0x0DE1 */

/* TextureUnit */
const GL_TEXTURE0                       0x84C0
const GL_TEXTURE1                       0x84C1
const GL_TEXTURE2                       0x84C2
const GL_TEXTURE3                       0x84C3
const GL_TEXTURE4                       0x84C4
const GL_TEXTURE5                       0x84C5
const GL_TEXTURE6                       0x84C6
const GL_TEXTURE7                       0x84C7
const GL_TEXTURE8                       0x84C8
const GL_TEXTURE9                       0x84C9
const GL_TEXTURE10                      0x84CA
const GL_TEXTURE11                      0x84CB
const GL_TEXTURE12                      0x84CC
const GL_TEXTURE13                      0x84CD
const GL_TEXTURE14                      0x84CE
const GL_TEXTURE15                      0x84CF
const GL_TEXTURE16                      0x84D0
const GL_TEXTURE17                      0x84D1
const GL_TEXTURE18                      0x84D2
const GL_TEXTURE19                      0x84D3
const GL_TEXTURE20                      0x84D4
const GL_TEXTURE21                      0x84D5
const GL_TEXTURE22                      0x84D6
const GL_TEXTURE23                      0x84D7
const GL_TEXTURE24                      0x84D8
const GL_TEXTURE25                      0x84D9
const GL_TEXTURE26                      0x84DA
const GL_TEXTURE27                      0x84DB
const GL_TEXTURE28                      0x84DC
const GL_TEXTURE29                      0x84DD
const GL_TEXTURE30                      0x84DE
const GL_TEXTURE31                      0x84DF

/* TextureWrapMode */
const GL_REPEAT                         0x2901
const GL_CLAMP_TO_EDGE                  0x812F

/* PixelInternalFormat */
const GL_COLOR_INDEX8_EXT               0x80E5

/* VertexPointerType */
/* const GL_FLOAT                          0x1406 */

/* Paletted Textures Extension */
const GL_COLOR_TABLE_FORMAT_EXT         0x80D8
const GL_COLOR_TABLE_WIDTH_EXT          0x80D9
const GL_COLOR_TABLE_RED_SIZE_EXT       0x80DA
const GL_COLOR_TABLE_GREEN_SIZE_EXT     0x80DB
const GL_COLOR_TABLE_BLUE_SIZE_EXT      0x80DC
const GL_COLOR_TABLE_ALPHA_SIZE_EXT     0x80DD
const GL_COLOR_TABLE_LUMINANCE_SIZE_EXT 0x80DE
const GL_COLOR_TABLE_INTENSITY_SIZE_EXT 0x80DF

/*************************************************************/

GLAPI void APIENTRY glActiveTexture (GLenum texture);
GLAPI void APIENTRY glAlphaFunc (GLenum func, GLclampf ref);
GLAPI void APIENTRY glBegin(GLenum mode);
GLAPI void APIENTRY glBindTexture (GLenum target, GLuint texture);
GLAPI void APIENTRY glBitmap (GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, GLfloat ymove, const GLubyte *bitmap);
GLAPI void APIENTRY glBlendFunc (GLenum sfactor, GLenum dfactor);
GLAPI void APIENTRY glCallLists (GLsizei n, GLenum type, const GLvoid *lists);
GLAPI void APIENTRY glClear (GLbitfield mask);
GLAPI void APIENTRY glClearColor (GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
GLAPI void APIENTRY glClearDepthf (GLclampf depth);
GLAPI void APIENTRY glClearStencil (GLint s);
GLAPI void APIENTRY glClientActiveTexture (GLenum texture);
GLAPI void APIENTRY glColor4f (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
GLAPI void APIENTRY glColor4fv (const GLfloat *v);
GLAPI void APIENTRY glColor4ub (GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha);
GLAPI void APIENTRY glColorMask (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
GLAPI void APIENTRY glColorPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
GLAPI void APIENTRY glColorSubTableEXT (GLenum target, GLsizei start, GLsizei count, GLenum format, GLenum type, const GLvoid *table);
GLAPI void APIENTRY glColorTableEXT (GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const GLvoid *table);
GLAPI void APIENTRY glCopyPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum type);
GLAPI void APIENTRY glCullFace (GLenum mode);
GLAPI void APIENTRY glDepthFunc (GLenum func);
GLAPI void APIENTRY glDepthMask (GLboolean flag);
GLAPI void APIENTRY glDepthRangef (GLclampf zNear, GLclampf zFar);
GLAPI void APIENTRY glDisable (GLenum cap);
GLAPI void APIENTRY glDisableClientState (GLenum array);
GLAPI void APIENTRY glDrawArrays (GLenum mode, GLint first, GLsizei count);
GLAPI void APIENTRY glDrawElements (GLenum mode, GLsizei count, GLenum type, const GLvoid *indices);
GLAPI void APIENTRY glDrawPixels (GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels);
GLAPI void APIENTRY glEnable (GLenum cap);
GLAPI void APIENTRY glEnableClientState (GLenum array);
GLAPI void APIENTRY glEnd (void);
GLAPI void APIENTRY glEndList (void);
GLAPI void APIENTRY glFinish (void);
GLAPI void APIENTRY glFlush (void);
GLAPI void APIENTRY glFrontFace (GLenum mode);
GLAPI void APIENTRY glFrustumf (GLfloat left, GLfloat right, GLfloat bottom, GLfloat top, GLfloat zNear, GLfloat zFar);
GLAPI GLuint APIENTRY glGenLists (GLsizei range);
GLAPI void APIENTRY glGenTextures (GLsizei n, GLuint *textures);
GLAPI GLenum APIENTRY glGetError (void);
GLAPI void APIENTRY glGetBooleanv (GLenum pname, GLboolean *params);
GLAPI void APIENTRY glGetColorTableEXT (GLenum target, GLenum format, GLenum type, GLvoid *table);
GLAPI void APIENTRY glGetColorTableParameterivEXT (GLenum target, GLenum pname, GLint *params);
GLAPI void APIENTRY glGetFloatv (GLenum pname, GLfloat *params);
GLAPI void APIENTRY glGetIntegerv (GLenum pname, GLint *params);
GLAPI void APIENTRY glGetLightfv (GLenum light, GLenum pname, GLfloat *params);
GLAPI void APIENTRY glGetMaterialfv (GLenum face, GLenum pname, GLfloat *params);
GLAPI void APIENTRY glGetPointerv (GLenum pname, GLvoid * *params);
GLAPI void APIENTRY glGetPolygonStipple (GLubyte *mask);
GLAPI void APIENTRY glGetTexEnvfv (GLenum target, GLenum pname, GLfloat *params);
GLAPI void APIENTRY glGetTexEnviv (GLenum target, GLenum pname, GLint *params);
GLAPI void APIENTRY glGetTexParameteriv (GLenum target, GLenum pname, GLint *params);
GLAPI const GLubyte * APIENTRY glGetString (GLenum name);
GLAPI void APIENTRY glHint (GLenum target, GLenum mode);
GLAPI GLboolean APIENTRY glIsEnabled (GLenum cap);
GLAPI void APIENTRY glLightfv (GLenum light, GLenum pname, const GLfloat *params);
GLAPI void APIENTRY glLightModelf (GLenum pname, GLfloat param);
GLAPI void APIENTRY glLightModelfv (GLenum pname, const GLfloat *params);
GLAPI void APIENTRY glLineStipple (GLint factor, GLushort pattern);
GLAPI void APIENTRY glLineWidth (GLfloat width);
GLAPI void APIENTRY glListBase (GLuint base);
GLAPI void APIENTRY glLoadIdentity (void);
GLAPI void APIENTRY glLoadMatrixf (const GLfloat *m);
GLAPI void APIENTRY glMaterialf (GLenum face, GLenum pname, GLfloat param);
GLAPI void APIENTRY glMaterialfv (GLenum face, GLenum pname, const GLfloat *params);
GLAPI void APIENTRY glMatrixMode (GLenum mode);
GLAPI void APIENTRY glMultMatrixf (const GLfloat *m);
GLAPI void APIENTRY glMultiTexCoord2f (GLenum target, GLfloat s, GLfloat t);
GLAPI void APIENTRY glNewList (GLuint list, GLenum mode);
GLAPI void APIENTRY glNormal3f (GLfloat nx, GLfloat ny, GLfloat nz);
GLAPI void APIENTRY glNormal3fv (const GLfloat *v);
GLAPI void APIENTRY glNormalPointer (GLenum type, GLsizei stride, const GLvoid *pointer);
GLAPI void APIENTRY glOrthof (GLfloat left, GLfloat right, GLfloat bottom, GLfloat top, GLfloat zNear, GLfloat zFar);
GLAPI void APIENTRY glPixelStorei (GLenum pname, GLint param);
GLAPI void APIENTRY glPointSize (GLfloat size);
GLAPI void APIENTRY glPolygonOffset (GLfloat factor, GLfloat units);
GLAPI void APIENTRY glPolygonStipple (const GLubyte *mask);
GLAPI void APIENTRY glPopMatrix (void);
GLAPI void APIENTRY glPushMatrix (void);
GLAPI void APIENTRY glRasterPos3f (GLfloat x, GLfloat y, GLfloat z);
GLAPI void APIENTRY glReadPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid *pixels);
GLAPI void APIENTRY glRotatef (GLfloat angle, GLfloat x, GLfloat y, GLfloat z);
GLAPI void APIENTRY glScalef (GLfloat x, GLfloat y, GLfloat z);
GLAPI void APIENTRY glScissor (GLint x, GLint y, GLsizei width, GLsizei height);
GLAPI void APIENTRY glShadeModel (GLenum mode);
GLAPI void APIENTRY glStencilFunc (GLenum func, GLint ref, GLuint mask);
GLAPI void APIENTRY glStencilMask (GLuint mask);
GLAPI void APIENTRY glStencilOp (GLenum fail, GLenum zfail, GLenum zpass);
GLAPI void APIENTRY glTexCoordPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
GLAPI void APIENTRY glTexEnvfv (GLenum target, GLenum pname, const GLfloat *params);
GLAPI void APIENTRY glTexEnvi (GLenum target, GLenum pname, GLint param);
GLAPI void APIENTRY glTexImage2D (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid *pixels);
GLAPI void APIENTRY glTexParameteri (GLenum target, GLenum pname, GLint param);
GLAPI void APIENTRY glTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels);
GLAPI void APIENTRY glTranslatef (GLfloat x, GLfloat y, GLfloat z);
GLAPI void APIENTRY glVertex2f (GLfloat x, GLfloat y);
GLAPI void APIENTRY glVertex2fv (const GLfloat *v);
GLAPI void APIENTRY glVertex3f (GLfloat x, GLfloat y, GLfloat z);
GLAPI void APIENTRY glVertex3fv (const GLfloat *v);
GLAPI void APIENTRY glVertexPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
GLAPI void APIENTRY glViewport (GLint x, GLint y, GLsizei width, GLsizei height);

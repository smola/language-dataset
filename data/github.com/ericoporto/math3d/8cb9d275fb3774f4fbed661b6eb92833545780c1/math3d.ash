// new module header
#define MAX_CELL_COUNT 16

enum MatrixType {
  eMT_None=0,
  eMT_Identity=1,
};

import float Abs(float f);
import String[] Split(this String*, String token);
import int CountToken(this String*, String token);

managed struct Vec3;

managed struct Quat {

  /// x of the quaternion (x,y,z,w).
  float x;

  /// y of the quaternion (x,y,z,w).
  float y;

  /// z of the quaternion (x,y,z,w).
  float z;

  /// w of the quaternion (x,y,z,w).
  float w;

  /// Creates a Quaternion.
  import static Quat* Create(float x=0, float y=0, float z=0, float w=0);

  /// Returns a string "(x, y, z, w)" for printing purposes.
  import readonly attribute String AsString;

  import String get_AsString(); // $AUTOCOMPLETEIGNORE$

  /// Returns a Vec3 representation of the Quaternion.
  import readonly attribute Vec3* AsVec3;

  /// Sets manually the values of the quaternion.
  import Quat* Set(float x=0, float y=0, float  z=0, float  w=0);

  /// Creates a new quaternion that is a copy of the cloned one.
  import Quat* Clone();

  /// Returns a new quaternion which is the sum of this quaternion with quaternion q.
  import Quat* Add(Quat* q);

  /// Returns a new quaternion which is the subtraction of this quaternion with quaternion q.
  import Quat* Sub(Quat* q);

  /// Returns a new quaternion which is the multiplication of this quaternion with quaternion q.
  import Quat* Mul(Quat* q);

  /// Returns a new quaternion which is the multiplication of this quaternion by a scalar s.
  import Quat* Scale(float s);

  /// Returns the Euclidean length of the quaternion.
  import readonly attribute float Length;

  import float get_Length(); // $AUTOCOMPLETEIGNORE$

  /// Returns a normalized copy of this quaternion. Normalize quaternion has length 1 but the same rotation.
  import readonly attribute Quat* Normalize;

  import Quat* get_Normalize(); // $AUTOCOMPLETEIGNORE$

  /// Returns the distance between this quaternion with quaternion q.
  import float Distance(Quat* q);

  /// Returns the dot multiplication of this quaternion with quaternion q.
  import float Dot(Quat* q);

  /// Returns the angle between this quaternion and quaternion q in radians.
  import float Angle(Quat* q);

  /// Linear interpolation by t percent of this quaternion and quaternion q.
  import Quat* Lerp(Quat* q,  float t);

  /// Returns a new quaternion with specified angle, maintaining axis.
  import Quat* setAngleAxis(float angle);

  /// Returns a new quaternion that is a copy of the current, normalized with x set to angle.
  import Quat* getAngleAxis();
};

managed struct Matrix {

  /// Don't modify this number directly. Each element of a matrix.
  float v[MAX_CELL_COUNT];

  /// Don't modify this number directly. The number of rows of a matrix.
  int row_count;

  /// Don't modify this number directly. The number of columns of a matrix.
  int column_count;

  /// Don't modify this number directly. The number of elements of a matrix.
  int cell_count;

  /// Sets the value of a specific row and column (y,x). This function modifyies the matrix you apply it directly.
  import Matrix* Set(int row, int column, float value);

  /// Returns the value of a specific row and column (similar to y,x position of a cell).
  import float Get(int row, int column);

  /// Returns a new matrix with defined rows and columns. You can set a value for all elements or make this matrix identity.
  import static Matrix* Create(int rows,  int columns,  MatrixType type=0,  float value=0);

  /// Returns a new 4x4 identity matrix. Functions preceeded with M44 require a matrix created with this method.
  import static Matrix* CreateM44();

  /// Reads a matrix as a string for printing purposes.
  import readonly attribute String AsString;

  import String get_AsString(); // $AUTOCOMPLETEIGNORE$

  /// Creates a matrix from a string like "{{0,4},{5,2}}".
  import static Matrix* CreateFromString(String s);

  /// Returns a new matrix that is a copy of this one.
  import Matrix* Clone();

  /// Returns true if all elements of the matrix m are equal to this one.
  import bool isEqual(Matrix* m);

  /// Returns a new matrix that is the sum of this matrix with a matrix m.
  import Matrix* Add(Matrix* m);

  /// Returns a new matrix that is the subtraction of this matrix with a matrix m.
  import Matrix* Sub(Matrix* m);

  /// Returns a new matrix that is the multiplication of this matrix with a matrix m.
  import Matrix* Mul(Matrix* m);

  /// Returns a new matrix that is the multiplication of this matrix with a scalar f.
  import Matrix* MulNum(float f);

  /// Returns a new matrix that is the division of this matrix with a scalar f.
  import Matrix* DivNum(float f);

  /// Returns a new matrix that is this matrix multiplied by itself n times. Matrix must be square.
  import Matrix* Pow(int n);

  /// Returns a the determinant of this matrix.
  import float Determinant();

  /// Returns the value of the biggest element of the matrix.
  import float MaxCell();

  /// Returns the value of the smallest element of the matrix.
  import float MinCell();

  /// Returns a point in homogeneous coordinates transformed. Has to be a 4x4 matrix. 
  import Quat* M44_DoTransform(float px, float py, float pz, float pw);

  /// Returns a point in homogeneous coordinates transformed. Has to be a 4x4 matrix. 
  import Quat* M44_DoTransformQuat(Quat* q);

  /// Returns the inverse of this matrix, as a new matrix. Has to be a 4x4 matrix. 
  import Matrix* M44_Invert();

  /// Returns a identity matrix, as a new matrix. Has to be a 4x4 matrix. 
  import Matrix* M44_SetIdentity();

  /// Returns copy of current matrix with translate set to passed position. Has to be a 4x4 matrix. 
  import Matrix* M44_SetTranslate(float x, float y, float z);

  /// Returns copy current matrix with set scaling. Has to be a 4x4 matrix. 
  import Matrix* M44_SetScale(float x, float y, float z);

  /// Returns copy current matrix with rotate set as passed. Has to be a 4x4 matrix. 
  import Matrix* M44_SetRotateEuler(float x, float y, float z);

  /// Returns copy current matrix with translate, rotate and scale set as passed. Has to be a 4x4 matrix. 
  import Matrix* M44_SetFullTransform(float x, float y, float z, float sx, float sy, float sz, float rx, float ry, float rz);

  /// Returns orthographic projection matrix with passed parameters. Has to be a 4x4 matrix.
  import Matrix* M44_SetOrthographicProjection(float left, float right, float bottom, float top, float near, float far);

  /// Returns perspective projection matrix with passed parameters. Has to be a 4x4 matrix.
  import Matrix* M44_SetPerspectiveProjection(float fovx, float fovy, float near, float far);
};


managed struct Vec3 {

  /// x of the triplet (x,y,z).
  float x;

  /// y of the triplet (x,y,z).
  float y;

  /// z of the triplet (x,y,z).
  float z;

  /// Creates a triplet (x,y,z) vector.
  import static Vec3* Create(float x=0, float y=0, float z=0);

  /// Returns a string "(x, y, z)" for printing purposes.
  import readonly attribute String AsString;

  import String get_AsString(); // $AUTOCOMPLETEIGNORE$

  /// Casts this triplet as a quaternion (w=1).
  import readonly attribute Quat* AsQuat;

  /// Set the values of the triplet.
  import Vec3* Set(float x=0, float y=0, float  z=0);

  /// Creates a new vector triplet that is a copy of the cloned one.
  import Vec3* Clone();

  /// Returns a new vector triplet which is the sum of this vector with a vector v.
  import Vec3* Add(Vec3* v);

  /// Returns a new vector triplet which is the sum of this vector with a quaternion q.
  import Vec3* AddQuat(Quat* q);

  /// Returns a new vector triplet which is the subtraction of this vector with a vector v.
  import Vec3* Sub(Vec3* v);

  /// Returns a new vector triplet which is the subtraction of this vector with a quaternion q.
  import Vec3* SubQuat(Quat* q);

  /// Returns a new vector triplet which is the multiplication of this vector with a vector v.
  import Vec3* Mul(Vec3* v);

  /// Returns a new vector triplet which is the multiplication of this vector with a quaternion q.
  import Vec3* MulQuat(Quat* q);

  /// Returns a new vector triplet which is the division of this vector with a vector v.
  import Vec3* Div(Vec3* v);

  /// Returns a new vector triplet which is the division of this vector with a quaternion q.
  import Vec3* DivQuat(Quat* q);

  /// Returns a new quaternion which is the multiplication of this quaternion by a scalar s.
  import Vec3* Scale(float s);

  /// Returns the length (distance to origin) of this vector.  
  import readonly attribute float Length;

  import float get_Length(); // $AUTOCOMPLETEIGNORE$

  /// Returns a copy of this vector, normalized.
  import readonly attribute Vec3* Normalize;

  import Vec3* get_Normalize(); // $AUTOCOMPLETEIGNORE$

  /// Returns the distance between this vector and a vector v.
  import float Distance(Vec3* v);

  /// Returns the distance between this vector and a quaternion q.
  import float DistanceQuat(Quat* q);

  /// Returns the dot multiplication of this vector and a vector v.
  import float Dot(Vec3* v);

  /// Returns the dot multiplication of this vector and a quaternion q.
  import float DotQuat(Quat* v);

  /// Returns the angle between this vector and a vector v.
  import float Angle(Vec3* v);

  /// Returns the angle between this vector and a quaternion q.
  import float AngleQuat(Quat* q);

  /// Returns a vector which is the cross multiplication of this vector with a vector v.
  import Vec3* Cross(Vec3* v);

  /// Returns a vector which is the cross multiplication of this vector with a quaternion q.
  import Vec3* CrossQuat(Quat* q);

  /// Returns a new vector which is the interpolation of percent t between this vector and a vector v.
  import Vec3* Lerp(Vec3* v,  float t);

  /// Returns a new vector that is equivalent of the projection of this vector on a vector v.
  import Vec3* Project(Vec3* v);

  /// Returns a new vector which is the rotate vector of this by a quaternion q.
  import Vec3* Rotate(Quat* q);
};

/// Casts a quaternion to a Vec3. You almost never wants to do this.
import Vec3* get_AsVec3(this Quat*);

/// Casts a Vec3 to quaternion. You almost never wants to do this.
import Quat* get_AsQuat(this Vec3*);

/// Sums a Vec3 from this quaternion. You almost never wants to do this.
import Quat* AddVec3(this Quat*, Vec3* v);

/// Subtracts a Vec3 from this quaternion. You almost never wants to do this.
import Quat* SubVec3(this Quat*, Vec3* v);

/// Multiplies this quaternion by a Vec3. You almost never wants to do this.
import Quat* MulVec3(this Quat*, Vec3* v);

/// Returns distance from this quaternion to a Vec3. You almost never wants to do this.
import float DistanceVec3(this Quat*, Vec3* v);

/// Returns distance the dot product between this quaternion and a Vec3. You almost never wants to do this.
import float DotVec3(this Quat*, Vec3* v);

/// Returns distance the angle between this quaternion and a Vec3. You almost never wants to do this.
import float AngleVec3(this Quat*, Vec3* v);

/// Returns interpolation between this quaternion and a Vec3. You almost never wants to do this.
import Quat* LerpVec3(this Quat*, Vec3* v,  float t);

managed struct ScreenPoint{
  int x;
  int y;
  float z;
  float w;
  bool is_visible;
};


struct Transform3D {

  int X;
  int Y;
  int Width;
  int Height;

  /// The height to section the top of the frustum geometric shape. Default is 1.0.
  float frustrum_near;

  /// The height to section the base of the frustum geometric shape. Default is 1000.0.
  float frustrum_far;

  /// The surface size height. Usually matches final viewport height.
  float surfsize_h;

  /// The surface size width. Usually matches final viewport width.
  float surfsize_w;

  /// The normalized device coordinates height. Default is 2.0.
  float ndcSize_h;

  /// The normalized device coordinates width. Default is 2.0.
  float ndcSize_w;

  /// The camera to world coordinates tranformation matrix.
  Matrix* CamToWorld;

  /// The world to camera coordinates tranformation matrix.
  Matrix* WorldToCam;

  /// The projection matrix.
  Matrix* ProjectMtx;

  /// Initialize a Transform3D object with default values. Call this after istanciating.
  import void Init();

  /// Sets the position of the viewport on screen.
  import void SetPosition( int x, int y, int  width, int height);

  /// Configures the camera transform, positioning the camera, scaling and rotating it.
  import void SetCameraTransform( Vec3* cam_pos, Vec3* cam_scale, Vec3* cam_rot);

  /// Sets the projection matrix for orthogonal projection.
  import void SetOrthoProjection(float width, float height, float near, float far);

  /// Sets the projection matrix for perspective projection. Field of View is in angle and specifies the frustum.
  import void SetPerspectiveProjection(float fov, float near, float far);

  /// Sets the surface size for the view used. This should match the viewport to avoid stretching the resulting image.
  import void SetSurfaceSize(float width, float height);

  /// Returns a world point converted to screen coordinates.
  import ScreenPoint* WorldToScreen(float x, float y, float z);

  //ScreenToWorld(x, y, z, result);
};


module m_mandel {
  record t_bmp_file_header {
    var bfType      : int(16) = 19778; //"BM"
    var bfSize      : int(32);
    var bfReserved1 : int(16) = 0;
    var bfReserved2 : int(16) = 0;
    var bfOffBits   : int(32);
  }

  record t_bmp_info_header {
    var biSize          : int(32) = 40; // Z'28'
    var biWidth         : int(32);
    var biHeight        : int(32);
    var biPlanes        : int(16) = 1;  // always 1
    var biBitCount      : int(16);
    var biCompression   : int(32) = 0;  // 0:nocompression 
    var biSizeImage     : int(32);
    var biXPelsPerMeter : int(32) = 3780; // 96 dpi
    var biYPelsPerMeter : int(32) = 3780; // 96 dpi
    var biClrUsed       : int(32) = 0;
    var biClrImportant  : int(32) = 0; 
  }

  record t_rgb {
    var b, g, r : uint(8);
  }

  class t_bmp {
    var D: domain(2);
    var file_header: t_bmp_file_header;
    var info_header: t_bmp_info_header;
    var rgb: [D] t_rgb; 

    proc wr(fn:string) {
      var f = open(fn, iomode.cw);
      var w = f.writer(kind=ionative);
      var nx: int(32) = this.D.high(1):int(32);
      var ny: int(32) = this.D.high(2):int(32);

      this.file_header.bfSize      = 14 + 40 + 0 + nx * ny * 3;
      this.file_header.bfOffBits   = 14 + 40;
      this.info_header.biWidth     = nx;
      this.info_header.biHeight    = ny;
      this.info_header.biBitCount  = 24;
      this.info_header.biSizeImage = nx * ny * 3;

      w.write(this.file_header.bfType     );
      w.write(this.file_header.bfSize     );
      w.write(this.file_header.bfReserved1);
      w.write(this.file_header.bfReserved2);
      w.write(this.file_header.bfOffBits  );

      w.write(this.info_header.biSize         );
      w.write(this.info_header.biWidth        );
      w.write(this.info_header.biHeight       );
      w.write(this.info_header.biPlanes       );
      w.write(this.info_header.biBitCount     );
      w.write(this.info_header.biCompression  );
      w.write(this.info_header.biSizeImage    );
      w.write(this.info_header.biXPelsPerMeter);
      w.write(this.info_header.biYPelsPerMeter);
      w.write(this.info_header.biClrUsed      );
      w.write(this.info_header.biClrImportant );

      for j in D.dim(2) do
        for i in D.dim(1) do
          w.write(this.rgb(i, j).b, this.rgb(i, j).g, this.rgb(i, j).r);
      w.close();  
    }
  }

  proc mandel(c:complex) {
    const maxiter : int = 255;
    var z: complex = c;
    var i, k: int;
    for i in 1..maxiter {
      k = i;
      if abs(z) > 2.0 then break; 
      z = z * z + c;
    }
    return k;     
  }  

  proc main() {
    const nx: int = 1440, ny: int = 1080;
    const x0: real = -2.0, y0: real = -1.5,
          x1: real =  2.0, y1: real =  1.5;

    var x: [1..nx] real, y: [1..ny] real;
    forall ix in 1..nx  do x(ix) = (x1 - x0) / (nx - 1) * (ix - 1) + x0;
    forall iy in 1..ny  do y(iy) = (y1 - y0) / (ny - 1) * (iy - 1) + y0;

    const D = {1..nx, 1..ny};
    var c: [D] complex;
    forall (ix, iy) in D  do c(ix, iy) = x(ix)+ y(iy) * 1.0i;

    var imandel: [D] int;
    imandel = mandel(c);

    var bmp = new t_bmp(D);
    bmp.rgb.r = (255 - imandel):uint(8);
    bmp.rgb.g = (255 - imandel):uint(8);
    bmp.rgb.b = (255 - imandel):uint(8);
    bmp.wr("test.bmp"); 
  }
}
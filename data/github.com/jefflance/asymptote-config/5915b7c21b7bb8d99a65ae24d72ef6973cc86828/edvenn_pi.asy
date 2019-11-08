// Copyright (c) 2007, Philippe Ivaldi.
// Version: $Id: edvenn_pi.asy,v 0.0 2007/02/19 17:49:31 Philippe Ivaldi Exp $

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or (at
// your option) any later version.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301, USA.

// Commentary:

// THANKS:

// BUGS:

// INSTALLATION:

// Code:


// Venn diagram // Diagramme de Venn
// Edwards' construction // Construction d'Edwards
import roundedpath;
size(10cm,0);
path [] EdVenn(int n)
{
  path [] opath;
  if (n>=1)
    opath.push(shift(-1.4,-.9)*roundedpath(xscale(2.8)*yscale(.9)*unitsquare,.1));
  if (n>=2)
    opath.push(shift(0,-.9)*roundedpath(xscale(1.4)*yscale(1.8)*unitsquare,.1));
  if (n>=3)
    opath.push(scale(.5)*unitcircle);
  for (int i=1; i<=n-3; ++i)
    {
      pair pcle=point(opath[2],1/(2^i)),
        ccle=intersectionpoint(pcle--(pcle-dir(opath[2],1/(2^i))), (0,0)--(1,0));
      path cle=shift(ccle)*scale(abs(pcle-ccle))*unitcircle;
      real[] p1=intersect(cle, opath[2]);
      path ocle=subpath(cle,-p1[0],p1[0]);
      guide tpath;
      real step=360/(2^i), a=0;
      for (int j=0; j<2^i; ++j)
        {
          tpath=tpath..rotate(a)*ocle;
          a+=step;
        }
      opath.push(tpath..cycle);      
    }
    return opath;
}

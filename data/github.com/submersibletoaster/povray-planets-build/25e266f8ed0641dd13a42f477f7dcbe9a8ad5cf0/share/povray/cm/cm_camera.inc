/*
#macro CubeLight(Face,Radius)
#local CubeSide = sqrt( 1/3 ) * (Radius*2);
union {
    
light_source { <0,CubeSide * 2,0> color rgb .5  }
light_source { <0,-CubeSide * 2,0> color rgb .5 }
light_source { <0,0,CubeSide * 2,> color rgb .5 }
light_source { <0,0,-CubeSide * 2,> color rgb .5 }
light_source { <CubeSide * 2,0,0> color rgb .5 }
light_source { <-CubeSide * 2,0,0> color rgb .5 }


  #switch (Face)
    #case (0)
      rotate -90*y
    #break
    #case (1)
      rotate 90*y
    #break
    #case (2)
      rotate z*180
      rotate 90*x
    #break
    #case (3)
        rotate z*180
        rotate -90 * x
    #break
    #case (4)
        rotate y*180
    #break
    #case (5)
      //rotate 180*y
    #break
  #end // End of conditional part

}
#end

*/

#macro CubeCamera(Face,Radius,UseBlur)
  #local CubeSide = sqrt( 1/3 ) * (Radius*2);
  
  //spherical 
  //angle 45,90
  
  perspective
  angle  90

   
  //orthographic
  
  location <0,0,0>
  up <0,CubeSide,0>
  right <CubeSide,0,0>
  direction <0,0,-CubeSide>

  
  //look_at <0,0,200>
  //right <1,0,0> up <0,1,0>
  #switch (Face)
    #case (0)
      rotate -90*y
    #break
    #case (1)
      rotate 90*y
    #break
    #case (2)
      rotate z*180
      rotate 90*x
    #break
    #case (3)
        rotate z*180
        rotate -90 * x
    #break
    #case (4)
        rotate y*180
    #break
    #break
    #case (5)
      //rotate 180*y
    #break
  #end // End of conditional part

#end

/*

#macro PigmentCameraFace(Face,Radius)
    camera_view {
        CubeCamera(Face,Radius,1)
        output 0
    }
#end



#macro CubeCameraOutside(Face,Radius)
  #local CubeSide = sqrt( 1/3 ) * (Radius*2);
  
  spherical 
  angle 40,40
  
  //perspective
  //angle  40
   
  //orthographic
  
  location <0,0,1>
  up <0,CubeSide,0>
  right <CubeSide,0,0>
  direction <0,0,-CubeSide>
  
  //look_at <0,0,200>
  //right <1,0,0> up <0,1,0>
  #switch (Face)
    #case (0)
      rotate -90*y
    #break
    #case (1)
      rotate 90*y
    #break
    #case (2)
      rotate z*180
      rotate 90*x
    #break
    #case (3)
        rotate z*180
        rotate -90 * x
    #break
    #case (4)
        rotate y*180
    #break
    #case (5)
      //rotate 180*y
    #break
  #end // End of conditional part

#end


#macro PigmentCameraFaceOutside(Face,Radius)
    camera_view {
        CubeCameraOutside(Face,Radius)
    }
#end




#macro CubeMapBoxes(Radius)
box { <0,0,0> <1,1,1> 
    texture { 
        pigment { PigmentCameraFace(0,Radius) }
        finish { ambient 1 } 
     }
     translate y*1005
     
}

box { <0,0,0> <1,1,1> 
    texture { 
        pigment { PigmentCameraFace(1,Radius) } 
        finish { ambient 1 } 
    }
    translate y*1004
}

box { <0,0,0> <1,1,1> 
    texture { 
        pigment { PigmentCameraFace(2,Radius) } 
        finish { ambient 1 } 
    }
    translate y*1003
}

box { <0,0,0> <1,1,1> 
    texture { 
        pigment { PigmentCameraFace(3,Radius) } 
        finish { ambient 1 } 
    }
    translate y*1002
}

box { <0,0,0> <1,1,1> 
    texture { 
        pigment { PigmentCameraFace(4,Radius) } 
        finish { ambient 1 } 
    }
    translate y*1001
}

box { <0,0,0> <1,1,1> 
    texture { 
        pigment { PigmentCameraFace(5,Radius) } 
        finish { ambient 1 } 
    }
    translate y*1000
}
#end



        
        
#macro CubeMapBoxesOutside(Radius)
box { <0,0,0> <1,1,1> 
    texture { 
        pigment { PigmentCameraFaceOutside(0,Radius) }
        finish { ambient 1 } 
     }
     translate y*1005
     
}

box { <0,0,0> <1,1,1> 
    texture { 
        pigment { PigmentCameraFaceOutside(1,Radius) } 
        finish { ambient 1 } 
    }
    translate y*1004
}

box { <0,0,0> <1,1,1> 
    texture { 
        pigment { PigmentCameraFaceOutside(2,Radius) } 
        finish { ambient 1 } 
    }
    translate y*1003
}

box { <0,0,0> <1,1,1> 
    texture { 
        pigment { PigmentCameraFaceOutside(3,Radius) } 
        finish { ambient 1 } 
    }
    translate y*1002
}

box { <0,0,0> <1,1,1> 
    texture { 
        pigment { PigmentCameraFaceOutside(4,Radius) } 
        finish { ambient 1 } 
    }
    translate y*1001
}

box { <0,0,0> <1,1,1> 
    texture { 
        pigment { PigmentCameraFaceOutside(5,Radius) } 
        finish { ambient 1 } 
    }
    translate y*1000
}
#end

*/

#macro CubeMapCamera()
// Box/Cube camera
 
camera { 
    orthographic
    location <0.5,1003 , -1>
    look_at  <0.5,1003 , 0>
    up <0,6,0>
    right <0,0,1>
}

#end





#macro PreviewCamera()
camera {
    perspective
    location <0.25,0.25,4>
    look_at <0,0,0>
    angle 33
    up y
    right x
}

#end

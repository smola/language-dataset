
#pragma version(1)
#pragma rs java_package_name(com.example.fuf111.aestheticscore)//(aesthfeat)
#pragma rs_fp_relaxed

// RenderScript kernel that performs RGB2HSV conversion
uchar4 __attribute__((kernel)) hsv(uchar4 in)
{
    // ARGB
    //float4 rgba = rsUnpackColor8888(in);
    uchar4 tempP;
    uchar minRGB = min( in.r, min( in.g, in.b ) );
    uchar maxRGB = max( in.r, max( in.g, in.b ) );
    uchar deltaRGB = maxRGB - minRGB;

    if ( deltaRGB <= 0) {

        tempP.s0 = 0; // undefined ???
        tempP.s1 = 0;

    } else { // deltaRGB > 0 -> maxRGB > 0

        tempP.s1 = (255 * deltaRGB) / maxRGB;

        if (in.r >= maxRGB) {

            if( in.g > in.b ) {

                tempP.s0 = (30 * (in.g - in.b)) / deltaRGB;        // between yellow & magenta

            } else {

                tempP.s0 = 180 + (30 * (in.g - in.b)) / deltaRGB;

            }
        } else if (in.g >= maxRGB) {

            tempP.s0 = 60 + (30 * (in.b - in.r)) / deltaRGB;  // between cyan & yellow

        } else {

            tempP.s0 = 120 + (30 * (in.r - in.g)) / deltaRGB;  // between magenta & cyan

        }

        /*
        if (in.b >= maxRGB) {
            if( in.g > in.r ) {
                tempP.s0 = (char)(30 * (in.g - in.r) / deltaRGB);        // between yellow & magenta
            } else {
                tempP.s0 = (char)(180 + 30 * (in.g - in.r) / deltaRGB);
            }
        } else if (in.g >= maxRGB) {
            tempP.s0 = (char)(60 + 30 * (in.r - in.b) / deltaRGB);  // between cyan & yellow
        } else {
            tempP.s0 = (char)(120 + 30 * (in.b - in.g) / deltaRGB);  // between magenta & cyan
        }
        */
    }

    tempP.s2 = maxRGB;
    tempP.s3 = in.a;

    return tempP;
}


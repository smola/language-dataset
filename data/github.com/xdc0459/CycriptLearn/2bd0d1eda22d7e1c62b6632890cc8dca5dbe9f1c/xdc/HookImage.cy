//#!usr/bin/cycript

var gx_cacheImagePath = [NSHomeDirectory() stringByAppendingPathComponent:@"Documents/HookImg"];
[[NSFileManager defaultManager] createDirectoryAtPath:gx_cacheImagePath withIntermediateDirectories:YES attributes:nil error:nil];

// - (nullable instancetype)initWithData:(NSData *)data scale:(CGFloat)scale
// - (nullable instancetype)initWithData:(NSData *)data
// var imageOriginInitData;
// function hook_UIImage_initWithData() {var imageOriginInitData = UIImage.prototype['initWithData:'];UIImage.prototype['initWithData:'] = function(arg1) {var path = [new NSString initWithFormat:@"%@/%@", gx_cacheImagePath, [new NSUUID init].UUIDString, nil];NSLog(@"UIImage.prototype['initWithData:'] %@", path, nil);[arg1 writeToFile:path atomically:YES]; return imageOriginInitData.call(this, arg1);}}
function gx_hook_UIImage_initWithData() {
    imageOriginInitData = UIImage.prototype['initWithData:'];
    UIImage.prototype['initWithData:'] = function(arg1) {
        var result = imageOriginInitData.call(this, arg1);
        if (result != nil) {
            var path = [new NSString initWithFormat:@"%@/%@", gx_cacheImagePath, [new NSUUID init].UUIDString, nil];
            NSLog(@"UIImage.prototype['initWithData:'] %@", path, nil);
            [arg1 writeToFile:path atomically:YES]; 
        }
        return result;
    }
}

// - (nullable instancetype)initWithData:(NSData *)data scale:(CGFloat)scale
function gx_hook_UIImage_initWithDataScale() {
    imageOriginInitDataScale = UIImage.prototype['initWithData:scale:'];
    UIImage.prototype['initWithData:scale:'] = function(arg1, arg2) {
        var result = imageOriginInitDataScale.call(this, arg1, arg2);
        if (result != nil) {
            var path = [new NSString initWithFormat:@"%@/%@", gx_cacheImagePath, [new NSUUID init].UUIDString, nil];
            NSLog(@"UIImage.prototype['initWithData:scale:'] %@", path, nil);
            [arg1 writeToFile:path atomically:YES]; 
        }
        return result;
    }
}
		   
// NSData  initWithContentsOfFile:(NSString *)path options:(NSDataReadingOptions)readOptionsMask error:(NSError **)errorPtr
function gx_hook_NSData_initWithContentsOfFileOptionError() {
    dataOriginInitFileOptionError = NSData.prototype['initWithContentsOfFile:options:error:'];
    NSData.prototype['initWithContentsOfFile:options:error:'] = function(arg1, arg2, arg3) {
        var result = dataOriginInitFileOptionError.call(this, arg1, arg2, arg3);
        if (result != nil) {
             NSLog(@"NSData.prototype['initWithContentsOfFile:options:error:'] %@", arg1, nil);
        }
        return result;
    }
}

function gx_hook_NSData_initWithContentsOfFile() {
    dataOriginInitFile = NSData.prototype['initWithContentsOfFile:'];
    NSData.prototype['initWithContentsOfFile:'] = function(arg1) {
        var result = dataOriginInitFile.call(this, arg1);
        if (result != nil) {
            NSLog(@"NSData.prototype['initWithContentsOfFile:'] %@", arg1, nil);
        }
        return result;
    }
}

// GPUImagePicture
// - (id)initWithCGImage:(CGImageRef)newImageSource smoothlyScaleOutput:(BOOL)smoothlyScaleOutput removePremultiplication:(BOOL)removePremultiplication
function gx_hook_GPUImagePicture_initWithCGImage() {
    filterOriginSetTexture = GPUImageFilter.prototype['initWithCGImage:smoothlyScaleOutput:removePremultiplication:'];
    GPUImageFilter.prototype['initWithCGImage:smoothlyScaleOutput:removePremultiplication:'] = function(arg1, arg2, arg3) {
        filterOriginSetTexture.call(this, arg1, arg2, arg3);
        if (arg1 != nil) {
            var path = [new NSString initWithFormat:@"%@/%@_%@.png", gx_cacheImagePath, NSStringFromClass([this class]), [NSDate date], nil];
            [UIImagePNGRepresentation(arg1) writeToFile:path atomically:YES]
        }
    }
}

//
function gx_hook_GPUImageFilter_setInputTexture() {
    filterOriginSetTexture = GPUImageFilter.prototype['setInputTextureformImage:atIndex:'];
    GPUImageFilter.prototype['setInputTextureformImage:atIndex:'] = function(arg1, arg2) {
        filterOriginSetTexture.call(this, arg1, arg2);
        if (arg1 != nil) {
            var path = [new NSString initWithFormat:@"%@/%@_%zi_%@.png", gx_cacheImagePath, NSStringFromClass([this class]), arg2, [NSDate date], nil];
            [UIImagePNGRepresentation(arg1) writeToFile:path atomically:YES]
        }
    }
}

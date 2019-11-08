module auth

in "C header" `{
    #include <stdio.h>
    #include <errno.h>
`}

# Fonction pour déterminer s'il est nécessaire de s'authentifier pour lire
# le fichier
fun reqAuth ( path : NativeString ) : Int `{
    char *mode = "r";
    FILE *fp = fopen( path, mode );

    if( fp != NULL ) {
        fclose( fp );
        return 200;
    } else {
        if ( errno == ENOENT ) {
            return 404;
        } else if ( errno == EACCES ) {
            return 403;
        }
    }
`}

#fun checkUser

#
fun authentifier: Bool
do
    var headers = [ "TODO" ] # get headers
    
    # Si header contient un champ Auth
    if headers.has( "Auth" ) then 
        
        return true
    else

        return false
end

end

# print reqAuth( "./test".to_cstring )

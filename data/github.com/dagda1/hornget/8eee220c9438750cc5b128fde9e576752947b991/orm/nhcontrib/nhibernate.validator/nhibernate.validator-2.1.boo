install nhibernate.validator:
    description "NHibernate Validator."
    get_from svn("https://nhcontrib.svn.sourceforge.net/svnroot/nhcontrib/trunk/src/NHibernate.Validator/")
    build_with nant, buildfile("default.build"), FrameworkVersion35	

    switches:
        parameters "skip.tests=true"

    with:
        tasks clean,build

    generate_strong_key

    shared_library "lib"
    build_root_dir "build"

dependencies:
    dependency "castle.core"             		>> "1.2" >> "Castle.Core"
    dependency "castle.dynamicproxy"			>> "2.2" >> "Castle.DynamicProxy2"
    dependency "nhibernate"   				>> "2.1"	>> "NHibernate"
    dependency "nhibernate"   				>> "2.1"	>> "Iesi.Collections"
    dependency "nhibernate"   				>> "2.1"	>> "NHibernate.ByteCode.Castle"

package.category = "ORM"
package.description = "NHibernate Validator."
package.forum = "http://groups.google.co.uk/group/nhusers?hl=en"
package.homepage = "http://www.ohloh.net/p/NHibernateContrib"
package.name = "NHibernate Validator"
package.notes = ""

''(`doctype("xml") `comment("Copyright Justin Marsh 2012. Released dual licensed under the GPLv3 and the D&R license. See LICENCE file for details.") manifest({"android:versionName" => "0.1", "android:versionCode" => 1, "package" => "net.flaviusb.twitatom", "xmlns:android" => "http://schemas.android.com/apk/res/android", "android:installLocation" => "auto"})
  ^usesSdk(android:minSdkVersion: "10")
  (application(android:icon: "@drawable/tweeticon", android:label: "@string/app_name")
    (activity(android:label: "@string/app_name", android:name: ".TwitAtom")
      (^intentFilter
        action(android:name: "android.intent.action.MAIN")
        category(android:name: "android.intent.category.LAUNCHER")))
    (activity(android:label: "@string/app_name", android:name: ".CreateShortcut") (^intentFilter
      action(android:name: "android.intent.action.CREATE_SHORTCUT"))))
    ^usesPermission(android:name: "android.permission.WRITE_EXTERNAL_STORAGE")
)

import UnityEngine
import System.Collections
import System.IO

class InputFolderChanger (MonoBehaviour): 

    public folderButton as GameObject
    public artBot as ArtBot
    public buttonText as Text
    private isOpen as bool

    public def Click():
        if isOpen:
            isOpen = false
            if transform.childCount > 0:
                arr = transform.GetComponentsInChildren[of Transform]()
                for child in arr:
                    if transform.childCount > 0:
                        DestroyImmediate(transform.GetChild(0).gameObject)
            gameObject.SetActive(false)

        else:
            isOpen = true
            gameObject.SetActive(true)
            ShowFolderContent("")

    public def ShowFolderContent(directoryName as string):
        directoryName = System.IO.Directory.GetCurrentDirectory() + directoryName
        folders = Directory.GetDirectories(directoryName)

        i as int = 0

        while i < folders.Length:
            clone = Instantiate(folderButton)
            clone.transform.SetParent(transform, false)

            fullName as string = folders[i].Remove(0, directoryName.Length+1)
            clone.transform.GetChild(0).GetComponent(Text).text = fullName

            i++

    public def SetInputFolder(newFolder as string):
    	artBot.inputFolder = newFolder
    	buttonText.text = newFolder
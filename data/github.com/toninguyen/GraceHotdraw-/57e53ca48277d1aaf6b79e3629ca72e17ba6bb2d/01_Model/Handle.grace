import "/Users/if0814/Dropbox/01_Master_Studium/04_Semester_NZ/03_Thesis/02_Implementation/01_DesignPattern/list" as list 
    
class Handle {
    method locate() {}
    method invokeStep(x, y, anchorX, anchorY, drawingView) {}
    method GetOwner() {}
    method getHandleDisplayBox(handle) {
        var p := handle.locate()
        return Rectangle (p.x - 8/2, p.y - 8/2, 8, 8)
    }

    method DrawHandle(handle Handle, graphics) {
        var rect := getHandleDisplayBox(handle)
        graphics.setFGColor("LightGray")
        graphics.drawBorderedRectFromRect(rect)
    }

}

class DefaultHandle (_figureOwner) {
	inherit Handle
    var figureOwner := _figureOwner

	method getOwner {
	    return self.owner
	}
}

class LocatorHandle (_figureOwner, _locator) {
	inherit DefaultHandle (_figureOwner)
	var locator (_locator)

	method getLocator {
	    return self.locator
	}

	//Locates the handle on the figure.
	//The handle is drawn centered around the returned point.
	method locate() {
	    return self.locator.locate(self.getOwner
	}
}

class Locator {
	method locate (figureOwner) {}
}

class RelativeLocator  {
	inherit Locator
	var relativeX is readable, writable
	var relativeY is readable, writable

	method newRelativeLocator {
		return newRelativeLocator(0.0, 0.0)
	}

	method newRelativeLocator(_relativeX, _relativeY) {
		var newRelativeLocatorObject := RelativeLocator
		newRelativeLocatorObject.relativeX := _relativeX
		newRelativeLocatorObject.relativeY := _relativeY
		return newRelativeLocatorObject
	}

	method locate(ownerFigure) {
		var rect := ownerFigure.getDisplayBox()
		var xValue := rect.x + rect.width * self.relativeX
		var yValue := rect.y + rect.height * self.relativeY
		return Point (xValue, yValue)
	}

	method createNorthLocator {
		return newRelativeLocator(0.5, 0.0)
	}

	method createWestLocator {
		return newRelativeLocator(0.0, 0.5)
	}

	method createSouthLocator {
		return newRelativeLocator(0.5, 1.0)
	}

	method createEastLocator {
		return newRelativeLocator(1.0, 0.5)
	}

	method createNorthEastLocator {
		return newRelativeLocator(1.0, 0.0)
	}

	method reateNorthWestLocator {
		return newRelativeLocator(0.0, 0.0)
	}

	method createSouthEastLocator {
		return newRelativeLocator(1.0, 1.0)
	}

	method createSouthWestLocator {
		return newRelativeLocator(0.0, 1.0)
	}

	method createCenterLocator {
		return newRelativeLocator(0.5, 0.5)
	}
}


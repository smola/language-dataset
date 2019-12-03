shh Given a time 'hh:mm', calculate the angle between the hour and minute hands.

such clockAngle much time
  very timeArr is time.split(":")
  very hour is plz parseInt with timeArr[0]
  very min is plz parseInt with timeArr[1]

  very minAngle is ((min/60) * 360) % 360
  very hourAngle is ((hour * 30) + (30 * min/60)) % 360

  very diff is minAngle - hourAngle
  very angle is plz Math.abs with diff

  wow angle

very testResult is plz clockAngle with '9:30'
plz console.loge with testResult
shh output -> 105

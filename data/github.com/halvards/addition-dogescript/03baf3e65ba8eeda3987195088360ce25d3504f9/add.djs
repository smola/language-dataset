shh https://dogescript.com/
quiet
  such set much theory
loud

trained

such add much numbersArray
  very positive is []
  very negative is []
  very radix is 10

  numbersArray dose forEach with much numberAsString
    very number is plz parseInt with numberAsString, radix
    rly number bigger 0
      positive is positive dose concat with Array(number)
    wow
    rly number smaller 0
      negative is negative dose concat with Array(Math.abs(number))
    wow
  wow&

  very positiveSliceNegative is positive dose slice with negative.length
  very negativeSlicePositive is negative dose slice with positive.length

  very result is Math dose max with positiveSliceNegative.length negativeSlicePositive.length

  rly negative.length bigger positive.length
    result is '-' dose concat with result
  but
    result is '' dose concat with result
  wow
wow result

module.exports is add

very numbersToAdd is process.argv dose slice with 2
very sum is plz add with numbersToAdd

console dose loge with sum

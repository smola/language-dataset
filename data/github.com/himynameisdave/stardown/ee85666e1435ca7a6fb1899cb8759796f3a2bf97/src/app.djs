trained
quiet
  this is my first dogescript!
  Such wow, very scripty!
loud

shh you will need to generate your own dang token!
very github is require('octonode')
very token is require('../token.json').token
very client is plz github.client with token

shh this fn returns a promise that resolves if it finds a gh user data and rejects if not a valid user/if error
very fetchStars is much username
  very promHandle is much res rej
    very dataHandle is much err status data
      rly err
        plz rej with err
      but
        plz res with data
      wow
    wow
    very userUrl is '/users/'+username+'/repos'
    plz client.get with userUrl dataHandle
  wow
  very prom is new Promise with promHandle
wow prom

shh this fn is for determining if a input element is empty or not
very isEmpty is much input
  very answer is false
  rly input.value is ''
    answer is true
  wow
wow answer

shh click handle
very clickHandle is much userOneInput userTwoInput e
  plz e.preventDefault
  rly isEmpty(userOneInput) or isEmpty(userTwoInput)
    plz alert with 'One or both username fields are empty!'
    return
  wow

  shh shh imma get a lil javascripty now
  Promise.all([ fetchStars(userOneInput.value), fetchStars(userTwoInput.value) ])
    .then(function(v){
      shh set the repo lengths and base total stars
      very userOneData is new Object
      userOneData.username is userOneInput.value
      userOneData.repoCount is v[0].length
      userOneData.totalStars is 0

      very userTwoData is new Object
      userTwoData.username is userTwoInput.value
      userTwoData.repoCount is v[1].length
      userTwoData.totalStars is 0

      v[0].forEach(function(repo){
        userOneData.totalStars += repo.stargazers_count
      });

      v[1].forEach(function(repo){
        userTwoData.totalStars += repo.stargazers_count
      });

      shh lets get the averageStar amount too
      userOneData.averageStars is userOneData.totalStars / userOneData.repoCount
      userTwoData.averageStars is userTwoData.totalStars / userTwoData.repoCount
      userOneData.averageStars is plz Math.round with userOneData.averageStars
      userTwoData.averageStars is plz Math.round with userTwoData.averageStars

      very resultsThusFar is new Object
      resultsThusFar.userOne is userOneData
      resultsThusFar.userTwo is userTwoData

      return resultsThusFar

    })
    .then(function(results){

      very DOM1 is new Object
      DOM1.name is plz dogeument.querySelector with '.js-userOne-name'
      DOM1.totalRepos is plz dogeument.querySelector with '.js-results-userOne-totalRepos'
      DOM1.totalStars is plz dogeument.querySelector with '.js-results-userOne-totalStars'
      DOM1.averageStars is plz dogeument.querySelector with '.js-results-userOne-averageStars'

      very DOM2 is new Object
      DOM2.name is plz dogeument.querySelector with '.js-userTwo-name'
      DOM2.totalRepos is plz dogeument.querySelector with '.js-results-userTwo-totalRepos'
      DOM2.totalStars is plz dogeument.querySelector with '.js-results-userTwo-totalStars'
      DOM2.averageStars is plz dogeument.querySelector with '.js-results-userTwo-averageStars'

      shh now set them, dogescript is ridic
      DOM1.name.innerHTML is results.userOne.username
      DOM1.totalRepos.innerHTML is results.userOne.repoCount
      DOM1.totalStars.innerHTML is results.userOne.totalStars
      DOM1.averageStars.innerHTML is results.userOne.averageStars

      DOM2.name.innerHTML is results.userTwo.username
      DOM2.totalRepos.innerHTML is results.userTwo.repoCount
      DOM2.totalStars.innerHTML is results.userTwo.totalStars
      DOM2.averageStars.innerHTML is results.userTwo.averageStars

      very results is plz dogeument.querySelector with '.js-results'
      plz results.classList.remove with 'results-s-invisible'
      plz results.classList.add with 'results-s-visible'

    })
    .catch(function(err){
      plz alert with 'Uh ohes, somthing went wrong!'
      plz console.loge with err
    })

wow


shh heres the app
very app is much

  very button is plz dogeument.querySelector with '.js-form-button'
  very userOne is plz dogeument.querySelector with '.js-user-one'
  very userTwo is plz dogeument.querySelector with '.js-user-two'

  button.onclick is clickHandle.bind(null, userOne, userTwo)

wow

plz app

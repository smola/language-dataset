such MessageController much $scope $rootScope $timeout
    $scope.isDisplayMessage is false
    $scope.status is ''
    $scope.body is ''

    such hidden much
        $scope.isDisplayMessage = false
    wow

    plz $rootScope.$on with 'message' much event params
        plz console.warn with 'on message' params

        $scope.isDisplayMessage is true
        $scope.status is params.status
        $scope.body is params.body

        plz $timeout with hidden 2000&
    wow&
wow

module.exports is MessageController

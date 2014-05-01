
var chainsApp = angular.module('chainsApp', []);

chainsApp.controller('chainsCtrl', function($scope, $http) {
    $http.get('chains.json').success(function(data) {
        $scope.chains = data
    })

    $scope.doneChain = function(name) {
        $scope.chains[name.toLowerCase()].progress.push(true)
        //console.log($scope.chains)
    }

    $scope.undoChain = function (name, index) {
        $scope.chains[name.toLowerCase()].progress[index] = false
        console.log($scope.chains)
    }
})

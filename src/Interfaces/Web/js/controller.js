
var chainsApp = angular.module('chainsApp', []);

chainsApp.controller('chainsCtrl', function($scope, $http) {
    $http.get('chains.json').success(function(data) {
        $scope.chains = data
    })

    $scope.saveData = function() {
        $http.post('save', $scope.chains).success(function() {
            console.log('saved')
        })
    }

    $scope.doneChain = function(name) {
        console.log($scope.chains.devel.progress)

        i = $scope.chains[name.toLowerCase()].progress.length
        $scope.chains[name.toLowerCase()].progress.push([i, true])
        $scope.saveData()
    }

    $scope.changeChain = function (name, index) {
        old = $scope.chains[name.toLowerCase()].progress[index]
        $scope.chains[name.toLowerCase()].progress[index] = [old[0], !old[1]]
        //console.log($scope.chains)
        $scope.saveData()
    }

})



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

    $scope.addChain = function (rawname) {
        name = rawname.toLowerCase()
        $scope.chains[name] = {}
        $scope.chains[name].name = rawname
        $scope.chains[name].progress = []
        $scope.chains[name].streak = 0
        $scope.chains[name].running = true
        $scope.chains[name].start = new Date().getTime().toString()
        $scope.chains[name].end = null
        //$scope.chains[name].start = 
        $scope.saveData()

        console.log($scope.chains)
    }

    $scope.doneChain = function(name) {
        i = $scope.chains[name.toLowerCase()].progress.length
        $scope.chains[name.toLowerCase()].progress.push([i, true])
        $scope.chains[name.toLowerCase()].streak += 1
        $scope.saveData()
    }

    $scope.changeChain = function (name, index) {
        old = $scope.chains[name.toLowerCase()].progress[index]
        $scope.chains[name.toLowerCase()].progress[index] = [old[0], !old[1]]
        
        streak = 0
        progress = $scope.chains[name.toLowerCase()].progress
        progress.reverse()

        for (i=0; i < progress.length; i++) {
            if (progress[i][1])
                streak += 1
            else
                break
        }

        progress.reverse()

        $scope.chains[name.toLowerCase()].streak = streak
        $scope.saveData()
    }

})


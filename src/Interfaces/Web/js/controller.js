
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

    $scope.addChain = function (name) {
        chain = $scope.chains[name.toLowerCase()] = {}

        chain.name = name
        chain.progress = []
        chain.streak = 0
        chain.running = true
        chain.start = new Date().getTime().toString()
        chain.end = null
        
        $scope.saveData()

        console.log($scope.chains)
    }

    $scope.doneChain = function(name) {
        chain = $scope.chains[name.toLowerCase()]
        i = chain.progress.length
        chain.progress.push([i, true])
        chain.streak += 1

        $scope.saveData()
    }

    $scope.changeChain = function (name, index) {
        chain = $scope.chains[name.toLowerCase()]
        old = chain.progress[index]
        chain.progress[index] = [old[0], !old[1]]
        
        streak = 0
        chain.progress.reverse()

        for (i=0; i < chain.progress.length; i++) {
            if (chain.progress[i][1])
                streak += 1
            else
                break
        }

        chain.progress.reverse()

        chain.streak = streak
        $scope.saveData()
    }

})


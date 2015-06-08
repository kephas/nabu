var nabuApp = angular.module('nabuApp', []);

nabuApp.controller('unitsCtrl', function($scope, $http) {
    $http.get('/units.json').success(function(data) { $scope.shellList = data; });
});

var nabuApp = angular.module('nabuApp', []);

nabuApp.controller('unitsCtrl', function($scope, $http) {
    $scope.refresh = function () {
	$http.get('/units.json').success(function(data) { $scope.shellList = data; });
    };

    $scope.refresh();
});

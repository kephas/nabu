var nabuApp = angular.module('nabuApp', [])

.controller('unitsCtrl', function($scope, $http) {
    $scope.refresh = function () {
	$http.get('/units.json').success(function(data) { $scope.shellList = data; });
    };

    $scope.alerts = [];

    $scope.dismiss = function(id) {
	var max = $scope.alerts.length;
	var rec = function(pos) {
	    if(pos < max) {
		if($scope.alerts[pos].id == id) {
		    $scope.alerts.splice(pos, 1);
		} else {
		    rec(pos + 1);
		}
	    }
	};
	rec(0);
    };

    $scope.refresh();
})

.directive('nabuAlerts', function() {
    return {
	restrict: 'E',
	templateUrl: '/static/ng/alerts.html'
    }
});

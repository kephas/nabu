var nabuApp = angular.module('nabuApp', ['nabuAlerts'])

    .controller('unitsCtrl', ['$scope', '$http', 'alerts', function($scope, $http, alerts) {
	$scope.refresh = function () {
	    $http.get('/units.json').success(function(data) { $scope.shellList = data; });
	};

	alerts.makeAvailable($scope);

	$scope.refresh();
    }]);

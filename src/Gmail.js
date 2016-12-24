var gmail = {
  users: function (callback) {
    callback(42);
  }
};

exports.users = function(callback) {
  return function() {
    gmail.users(function(p) {
      callback(p)();
    });
  };
};

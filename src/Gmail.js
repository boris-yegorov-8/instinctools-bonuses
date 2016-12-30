var gmail = {
  users: function (callback) {
    callback(42);
  }
};

exports.users = function(oauth2Client) {
  console.log('oauth2Client');
  console.log(oauth2Client);
  console.log('oauth2Client');
  return function(callback) {
    return function() {
      gmail.users(function(p) {
        callback(p)();
      });
    };
  };
};

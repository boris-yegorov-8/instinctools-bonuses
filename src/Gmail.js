var gmail = require('googleapis').gmail('v1');

exports.getMessages = function(options) {
  return function(success, error) {
    gmail.users.messages.list(options, function(err, response) {
      if (err) {
        error(err);
      } else {
        success((response || {}).messages || []);
      }
    });
  };
};

exports.getMessage = function (userId) {
  return function (messageId) {
    return function(success, error) {
      gmail.users.messages.list({ userId: userId, id: messageId }, function(err, response) {
        if (err) {
          error(err);
        } else {
          console.log('------------');
          console.log(response);
          console.log('-----------');
          success(42);
        }
      });
    };
  };
};

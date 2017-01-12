var gmail = require('googleapis').gmail('v1');

exports.getMessages = function(options) {
  return function(callback) {
    return function() {
      gmail.users.messages.list(options, function(err, response) {
        if (err) {
          console.log('The API returned an error');
        } else {
          console.log(response.messages);
        }
        callback(42)(response.messages || [])();
      });
    };
  };
};

// var gmail = require('googleapis').gmail('v1');
var gmail = function (callback) {
  callback(42, 73);
}

exports.getMessages = function(options) {
  return function(callback) {
    return function() {
      gmail.users(function(p) {
        callback(p)();
      });
    };
  };
};
// gmail.users.messages.list(options, function(err, response) {
//   if (err) {
//     console.log('The API returned an error');
//     return;
//   }
//   console.log(response.messages);
// });

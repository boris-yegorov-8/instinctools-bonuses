var googleAuth = require('google-auth-library');

exports.createClient = function(options) {
  var auth = new googleAuth();

  var oauth2Client = new auth.OAuth2(options.clientId, options.clientSecret, options.redirectUrl);

  oauth2Client.credentials = options.token;

  return oauth2Client;
};

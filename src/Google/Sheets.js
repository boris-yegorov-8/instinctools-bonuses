var sheets = require('googleapis').sheets('v4');

exports.getValues = function(options) {
  return function(success, error) {
    sheets.spreadsheets.values.get(options, function(err, response) {
      if (err) {
        error(err);
      } else {
        success(response);
      }
    });
  };
};

exports.batchUpdate = function(options) {
  return function(success, error) {
    console.log('-------------');
    // console.log(JSON.parse(options.resource));
    console.log(options.resource);
    console.log('-------------');
    sheets.spreadsheets.batchUpdate(
      {
        auth: options.auth,
        spreadsheetId: options.spreadsheetId,
        resource: JSON.parse(options.resource),
      },
      function(err) {
        if (err) {
          error(err);
        } else {
          success();
        }
      }
    );
  };
};

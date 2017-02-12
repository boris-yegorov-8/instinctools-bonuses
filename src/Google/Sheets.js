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
    // console.log('-------------');
    // console.log(options.resource.requests[1].updateCells.fields);
    // console.log('-------------');
    sheets.spreadsheets.batchUpdate(options, function(err) {
      if (err) {
        error(err);
      } else {
        success();
      }
    });
  };
};

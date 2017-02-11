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
    // TODO: move request to purs
    // var request = {
    //   auth: options.auth,
    //   spreadsheetId: options.spreadsheetId,
    //   resource: {
    //     requests: [
          // {
          //   updateCells: {
          //     start: { sheetId: 0, rowIndex: 156, columnIndex: 0 },
          //     rows: [
          //       {
          //         values: [
          //           {
          //             userEnteredValue: { numberValue: 1 },
          //           }
          //         ]
          //       }
          //     ],
          //     fields: 'userEnteredValue'
          //   }
          // },
          // {
          //   copyPaste: {
          //     source: {
          //       sheetId: 0,
          //       startRowIndex: 152,
          //       endRowIndex: 153,
          //       startColumnIndex: 7,
          //       endColumnIndex: 8
          //     },
          //     destination: {
          //       sheetId: 0,
          //       startRowIndex: 153,
          //       endRowIndex: 154,
          //       startColumnIndex: 7,
          //       endColumnIndex: 8
          //     },
          //     pasteType: 'PASTE_FORMULA'
          //   }
          // },
    //     ]
    //   }
    // };
    // console.log('----------');
    // console.log(JSON.stringify(options.resource.requests));
    // console.log('-----------');
    sheets.spreadsheets.batchUpdate(
      {
        auth: options.auth,
        spreadsheetId: options.spreadsheetId,
        resource: {
          requests: options.resource.requests.map(function (request) { return request.value0; }),
        },
      },
      function(err) {
        if (err) {
          error(err);
        } else {
          success('42');
        }
      }
    );
  };
};

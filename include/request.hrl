-record(objs, {
          eqptType :: string(),
          eqptIdCode :: string(),
          eqptPwd :: string(),
          resDataType = "" :: string(),
          cmdType :: string(),
          cmdId :: string(),
          cmdData = "" :: string()
         }).
-type objs() :: #objs{}.

-record(req_obj, {
          orderNumber :: string(),
          partner :: string(),
          objs :: objs(),
          sign :: string()
         }).
-type req_obj() :: #req_obj{}.

-record(req_rd, {
          pid :: pid(),
          soLibName :: string(),
          reqObj :: req_obj()
         }).
-type req_rd() :: #req_rd{}.

-record(input_rd, {
          meterAddress :: string(),
          collector2 :: string(),
          collector1 :: string(),
          info :: string(),
          infoDataLen :: integer(),
          route1 :: string(),
          route2 :: string(),
          isBCD :: string(),
          subSEQ :: integer(),
          pwd :: string()
         }).
-type input_rd() :: #input_rd{}.

-record(output_rd, {
          data :: string(),
          frame :: string(),
          error :: string(),
          dataLen :: integer(),
          frameLen :: integer(),
          errorLen :: integer(),
          funcResult :: integer()
}).
-type output_rd() :: #output_rd{}.

-record(req_task, {          
          taskStatus :: integer(),
          timerRef :: undefined | reference(),
          inputRd :: input_rd(),
          outputRd :: output_rd(),
          reqRd :: req_rd()
         }).

-type req_task() :: #req_task{}.

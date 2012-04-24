%% use by the client to create the request
-record(spay_request, {
          type,
          machine_id,
          phone_number,
          provider
}).

%%
-record(spay_dyndata,
        {
          seq
        }
).

-record(spay,
         {
          machine_id
         }
       ).


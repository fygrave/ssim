Scalable Correlation Server for OSSIM (Open Source Security Information Management) Framework
===


This is an erlang/OTP based implementation of OSSIM event collection and correlation server.
It is indended to work directly with ossim-agent and does not require any additional OSSIM
 components (database, framework, UI are not needed).

The riak is being used as primarily storage system and RabbitMQ is used as messaging platform
between system components. Both components should allow endless horisontal scalability.

You will need:
    ossim-agent (http://alienvalut.com/download-ossim)
    riaksearch (http://wiki.basho.com/Riak-Search.html)
    RabbitMQ (http://www.rabbitmq.com/)

Basic architecture:


{ossim-agent}
{ossim-agent}        => {ssim_agent_server}
                                        <=> {rabbitMQ} <=> {ssim_correlator} <==> {riaksearch}
....                                                                                +-[ UI ]
{ossim-agent}        =>{ssim_agent_server}
{ssim_syslog_server} => ....

It is possible to cluster instances of ssim_agent_servers and ssim_correlators

To build and start server:
   mkdir deps ebin
   ./rebar check-deps
   ./prefetch_rabbit
   ./rebar get-deps
   ./rebar compile
   mkdir rel
   ../rebar create-node nodeid=ssim
   cd ..
   cp reltool.config rel/
   cp priv/app.config.sample rel/files/app.config
   rebar generate
   cd rel/ssim/etc
   <edit app.config to point to your riaksearch cluster and rabbitmq server>
   cd rel/ssim/bin
   chmod a+x ssim
   ./ssim start


for any questions fygrave at o0o dot nu




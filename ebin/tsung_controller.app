{application,tsung_controller,
             [{vsn,"1.4.2"},
              {description,"tsung, a bench tool for TCP/UDP servers"},
              {id,[]},
              {modules,[]},
              {registered,[ts_stats,ts_mon,ts_config_server,ts_os_mon]},
              {applications,[kernel,stdlib,crypto,public_key,ssl,crypto]},
              {env,[{debug_level,6},
                    {smp_disable,true},
                    {ts_cookie,"humhum"},
                    {clients_timeout,60000},
                    {file_server_timeout,30000},
                    {warm_time,1},
                    {thinktime_value,"5"},
                    {thinktime_override,"false"},
                    {thinktime_random,"false"},
                    {munin_port,4949},
                    {snmp_port,161},
                    {snmp_version,v2},
                    {snmp_community,"public"},
                    {dumpstats_interval,10000},
                    {dump,none},
                    {stats_backend,none},
                    {nclients,10},
                    {nclients_deb,1},
                    {nclients_fin,2000},
                    {config_file,"./tsung.xml"},
                    {log_file,"./tsung.log"},
                    {match_log_file,"./match.log"}]},
              {mod,{tsung_controller,[]}},
              {start_phases,[{load_config,[]},
                             {start_os_monitoring,[{timeout,30000}]},
                             {start_clients,[]}]}]}.
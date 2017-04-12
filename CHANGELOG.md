0.1.0.3
-------
Server Management:
  - secured /metrics
  - configuration yaml supports server, management, logging configuration
      server:
        name:
        port:
      management:
        port:
        userName:
        userPassword:
      log:
        appenders:
        - Console
        request: Console

0.1.0.2
-------
Logging improvements: configure request log appender - None, Console, File (req.log)

0.1.0.1
-------
Logging improvements: log to console and file, ability to configure appenders in .yaml file, logging functions logMsg and logMsgH

0.1.0.0
-------

Initial code

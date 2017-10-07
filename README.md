
# emqx-lua-hook

This plugin make it possible to write hooks in lua scripts.

Lua virtual machine is implemented by [luerl](https://github.com/rvirding/luerl) which supports Lua 5.2. Following features may not work properly:
* label and goto
* tail-call optimisation in return
* only limited standard libraries
* proper handling of __metatable

For the supported functions, please refer to luerl's [project page](https://github.com/rvirding/luerl). 

Lua scripts are stored in hook_lua directory, and will be loaded automatically. If a script is changed during runtime, it should be reloaded to take effect. 

Each lua script could export several functions binding with emqx hooks, triggered by message publish, topic subscribe, client connect, etc. Different lua scripts may export same type function, binding with a same event. But their order being triggered is not guaranteed.   

To start this plugin, run following command:
```shell
bin/emqx_ctl plugins load emq_lua_hook
```


## NOTE

* Since lua VM is run on erlang VM, its performance is poor. Please do NOT write long or complicated lua scripts which may degrade entire system.
* It's hard to debug lua script in emqx environment. Recommended to unit test your lua script in your host first. If everything is OK, deploy it to empttd hook_lua directory.
* Global variable will lost its value for each call. Do NOT use global variable in lua scripts.



# Example

Suppose your emqx is installed in /emqx, and the lua script directory should be /emqx/hook_lua.

Make a new file called "test.lua" and put following code into this file:

```lua
function on_message_publish(clientid, username, topic, payload, qos, retain)
    return topic, "hello", qos, retain
end

function register_hook()
    return "on_message_publish"
end
```

Execute following command to start emq-lua-hook and load scripts in hook_lua directory.

```
/emqx/bin/emqx_ctl plugins load emqx_lua_hook
```

Now let's take a look at what will happend.

- Start a mqtt client, such as mqtt.fx. 
- Subscribe a topic="a/b".
- Send a message, topic="a/b", payload="123"
- Subscriber will get a message with topic="a/b" and payload="hello". test.lua modifies the payload.

If there are "test1.lua", "test2.lua" and "test3.lua" in /emqx/hook_lua, all these files will be loaded once emq-lua-hook get started.

If test2.lua has been changed, restart emq-lua-hook to reload all scripts, or execute following command to reload test2.lua only:
```
/emqx/bin/emqx_ctl luahook reload test2.lua
```



# Hook API

## on_message_publish

```lua
function on_message_publish(clientid, username, topic, payload, qos, retain)
    -- do your job here 
    if some_condition then
        return new_topic, new_payload, new_qos, new_retain
    else
        return false
    end
end
```
This API is called before publishing message into mqtt engine. It's possible to change message or cancel publish in this API.

### Input
* clientid : a string, mqtt client id of publisher.
* username : a string, mqtt username of publisher
* topic :   a string, mqtt message's topic
* payload :  a string, mqtt message's payload
* qos :     a number, mqtt message's QOS (0, 1, 2)
* retain :   a boolean, mqtt message's retain flag
### Output
* new_topic :   a string, change mqtt message's topic
* new_payload :  a string, change mqtt message's payload
* new_qos :      a number, change mqtt message's QOS
* new_retain :   a boolean, change mqtt message's retain flag
* false :        cancel publishing this mqtt message

## on_message_delivered 

```lua
function on_message_delivered(ClientId, Username, topic, payload, qos, retain)
    -- do your job here 
    return 0
```
This API is called after a message has been pushed to mqtt clients.

### Input
* ClientId : a string, mqtt client id.
* Username : a string mqtt username
* topic :   a string, mqtt message's topic
* payload :  a string, mqtt message's payload
* qos :     a number, mqtt message's QOS (0, 1, 2)
* retain :   a boolean, mqtt message's retain flag
### Output
* always be 0

## on_message_acked

```lua
function on_message_acked(ClientId, Username, Topic, Payload, Qos, Retain)
    return 0
end"
```
This API is called after a message has been acknowledged.

### Input
* ClientId : a string, mqtt client id.
* Username : a string mqtt username
* topic :   a string, mqtt message's topic
* payload :  a string, mqtt message's payload
* qos :     a number, mqtt message's QOS (0, 1, 2)
* retain :   a boolean, mqtt message's retain flag
### Output
* always be 0


## on_client_connected

```lua
function on_client_connected(ClientId, UserName, ReturnCode)
    return 0
nend
```
This API is called after a mqtt client has establish a connection with broker.

### Input
* ClientId : a string, mqtt client id.
* Username : a string mqtt username
* ReturnCode :   a number, has following values
    - 0 : Connection accepted
    - 1 : Unacceptable protocol version
    - 2 : Client Identifier is correct UTF-8 but not allowed by the Server
    - 3 : Server unavailable
    - 4 : Username or password is malformed
    - 5 : Client is not authorized to connect
### Output
* always be 0


## on_client_subscribe

```lua
function on_client_subscribe(ClientId, Username, Topic)
    -- do your job here
    if some_condition then
        return new_topic
    else
        return false
    end
end
```
This API is called before mqtt engine process client's subscribe command. It is possible to change topic or cancel it.

### Input
* ClientId : a string, mqtt client id.
* Username : a string mqtt username
* Topic :   a string, mqtt message's topic
### Output
* new_topic :   a string, change mqtt message's topic
* false :    cancel subscription


## on_client_unsubscribe

```lua
 function on_client_unsubscribe(ClientId, Username, Topic)
    -- do your job here
    if some_condition then
        return new_topic
    else
        return false
    end
end
```
This API is called before mqtt engine process client's unsubscribe command. It is possible to change topic or cancel it.

### Input
* ClientId : a string, mqtt client id.
* Username : a string mqtt username
* Topic :   a string, mqtt message's topic
### Output
* new_topic :   a string, change mqtt message's topic
* false :    cancel unsubscription


## on_client_disconnected

```lua
function on_client_disconnected(ClientId, Username, Error)
    return 0
end
```
This API is called after a mqtt client has disconnected.

### Input
* ClientId : a string, mqtt client id.
* Username : a string mqtt username
* Error :   a string, denote the disconnection reason.
### Output
* always be 0.


## on_session_subscribed

```lua
function on_session_subscribed(ClientId, Username, Topic)
    return 0
end
```
This API is called after a subscription has been done.

### Input
* ClientId : a string, mqtt client id.
* Username : a string mqtt username
* Topic :   a string, mqtt's topic filter.
### Output
* always be 0.


## on_session_unsubscribed

```lua
function on_session_unsubscribed(ClientId, Username, Topic)
    return 0
end
```
This API is called after a unsubscription has been done.

### Input
* ClientId : a string, mqtt client id.
* Username : a string mqtt username
* Topic :   a string, mqtt's topic filter.
### Output
* always be 0.


## register_hook
```lua
function register_hook()
    return "hook_name"
end

function register_hook()
    return "hook_name1", "hook_name2", ... , "hook_nameX"
end
```
This API exports hook(s) implemented in its lua script. 
### Output
* hook_name must be a string, which is equal to the hook API(s) implemented. Possible values:
 - "on_message_publish"
 - "on_message_delivered"
 - "on_message_acked"
 - "on_client_connected"
 - "on_client_subscribe"
 - "on_client_unsubscribe"
 - "on_client_disconnected"
 - "on_session_subscribed"
 - "on_session_unsubscribed"




# management command

## load

```shell
emqx_ctl luahook load script_name
```
This command will load lua file "script_name.lua" in hook_lua directory, into emqx hook.

## unload

```shell
emqx_ctl luahook unload script_name
```
This command will unload lua file "script_name.lua" out of emqx hook.

## reload

```shell
emqx_ctl luahook reload script_name
```
This command will reload lua file "script_name.lua" in hook_lua. It is useful if a lua script has been modified and apply it immediately.

## enable

```shell
emqx_ctl luahook enable script_name
```
This command will rename lua file "script_name.lua.x" to "script_name.lua", and load it immediately.

## disable

```shell
emqx_ctl luahook disable script_name
```
This command will unload this script, and rename lua file "script_name.lua" to "script_name.lua.x", which will not be loaded during next boot.







License
-------

Apache License Version 2.0

Author
------

Feng Lee <feng@emqtt.io>


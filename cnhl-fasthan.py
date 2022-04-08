from epc.server import EPCServer
from fastHan import FastHan

server = EPCServer(('localhost', 0))

global model

@server.register_function
def fasthan_init_model(modelType, path):
    global model
    if len(path) == 0:
        model = FastHan(model_type=modelType)
    else:
        model = FastHan(model_type=modelType, url=path)
        pass
    return 't'

@server.register_function
def fasthan_parsing_string(sentence):
    return model(sentence, 'Parsing')

server.print_port()
server.serve_forever()

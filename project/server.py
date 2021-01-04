import asyncio
import argparse
from time import time
import aiohttp
import json
import logging

API_key = '' #Need Google API key

connections = {
    'Hill': ['Jaquez','Smith'],
    'Jaquez': ['Hill','Singleton'],
    'Smith': ['Hill', 'Campbell','Singleton'],
    'Campbell': ['Smith','Singleton'],
    'Singleton': ['Jaquez','Smith','Campbell']
}

#ports given for the assignment
server_ports = {
    'Hill': 12380,
    'Jaquez': 12381,
    'Smith': 12382,
    'Campbell': 12383,
    'Singleton': 12384
}

class Server:
    def __init__(self, name, ip='127.0.0.1', port=12380, message_max_length=1e6):
        self.name = name
        self.ip = ip
        self.port = port
        self.message_max_length = int(message_max_length)
        self.clients = {}
        self.floodMessages = {}
        

    def parseLocation(self,location):
        for i in range(1,len(location)):
            if location[i] == '+' or location[i] == '-':
                return (location[:i],location[i:])
    
    async def processAT(self,message,args):
        if args[3] not in self.floodMessages or message != self.floodMessages[args[3]]:
            # self.floodMessages[args[1]] = message
            self.floodMessages[args[3]] = message
            self.clients[args[3]] = (args[4],args[5],args[2])
            await self.flood(message)

    async def processIAMAT(self, args, time):
        #time took for server to receive
        timeDifference = str(time - float(args[3]))
        if timeDifference[0].isnumeric():
            timeDifference = '+' + timeDifference
        #Add to list of known clients its location and time sent
        self.clients[args[1]] = (args[2], args[3], timeDifference)
        self.floodMessages[args[1]] = 'AT {0} {1} {2} {3} {4}'.format(self.name,timeDifference,args[1],args[2],args[3])
        return 'AT {0} {1} {2} {3} {4}'.format(self.name,timeDifference,args[1],args[2],args[3])

    #processes the Command WHATSAT
    async def processWHATSAT(self, args):
        location,startTime,timeDifference = self.clients[args[1]]
        lat,long = self.parseLocation(location)
        #Gets rid of '+' sign in latitude
        if lat[0] == '+':
            lat = lat[1:]
        if long[1] == '+':
            long = long[1:]

        #create URL for google places request
        urlLocationFormat = lat + ',' + long
        URL = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={0}&radius={1}&key={2}'.format(urlLocationFormat,int(args[2])*1000,API_key)

        async with aiohttp.ClientSession() as session:
            async with session.get(URL) as resp:
                json_file = await resp.json()

        # response = 'AT {0} {1} {2} {3} {4}\n'.format(self.name,timeDifference,args[1],location,startTime)
        response = self.floodMessages[args[1]] + '\n'
        json_file['results'] = json_file['results'][:int(args[3])]

        response += json.dumps(json_file,indent=3)
        response += '\n'
        
        return response
        
    #flooding algorithm to send messages to other servers
    async def flood(self, message):
        for server in connections[self.name]:
            logging.info('Attempting to connect to server {0} from server {1} to update location using flood Algorithm.'.format(server,self.name))
            try:
                reader, writer = await asyncio.open_connection(self.ip, server_ports[server])
                writer.write(message.encode())
                await writer.drain()
                writer.write_eof()
                writer.close()
                logging.info('Successfully connected and sent updated information to {0}.'.format(server))
            except:
                logging.info('Error connecting to server {0}: skipping flood...'.format(server))



    #Makes sure coordinates are in proper ISO notation
    def checkISOnotation(self,latAndLong):
        if latAndLong[0] != '+' and latAndLong[0] != '-':
            return None

        seenSign= False
        longStart = -1
        seenPeriod = False

        #Check the string to make sure it is valid coordinates
        for i in range(1,len(latAndLong)):
            if latAndLong[i] == '+' or latAndLong[i] == '-':
                if not seenSign:
                    longStart = i
                    seenSign = True
                    seenPeriod = False
                else:
                    return None
            elif latAndLong[i] == '.':
                if seenPeriod == False:
                    seenPeriod = True
                else:
                    return None
            elif not latAndLong[i].isnumeric() and latAndLong[i] != '.':
                return None
        
        # didn't see a second sign, sign occurred right after first, or sign happened at very end
        if longStart == -1 or longStart == 1 or longStart == len(latAndLong)-1:
            return None
        
        return longStart
        

    #checks if the commands have valid arguments
    def checkValid(self,args):
        numArgs = len(args)
        val = 1
        if numArgs < 4:
            val = -1
        elif args[0] == 'IAMAT':
            if self.checkISOnotation(args[2]) == None or numArgs != 4:
                val = -1
            try:
                float(args[3])
            except:
                val = -1
        elif args[0] == 'WHATSAT':
            if numArgs != 4:
                val = -1
            try:
                if float(args[2]) <= 0 or float(args[2]) > 50 or int(args[3]) <= 0 or int(args[3]) > 20:
                    val = -1
            except:
                val = -1
        else:
            val = -1
        return val

        
    async def handle_input(self,reader,writer):
        data = await reader.read(self.message_max_length)
        message = data.decode()
        
        # get the time
        receivedTime = time()
        #parse string into its different components
        parsed_message = message.strip().split()
        
        #Get the name of the command
        task = parsed_message[0]
        
        #Find the correct command
        if task == 'IAMAT':
            check = self.checkValid(parsed_message)
            if check != -1:
                response = await self.processIAMAT(parsed_message, receivedTime)
            else:
                response = '? ' + message  
        elif task == 'WHATSAT':
            check = self.checkValid(parsed_message)
            if check != -1 and parsed_message[1] in self.clients:
                response = await self.processWHATSAT(parsed_message)
            else:
                response = '? ' + message
        elif task == 'AT':
            await self.processAT(message,parsed_message)
        else:
            response = '? ' + message 
        
        #write back to the client and wait for client to read
        if task != 'AT':
            logging.info(response)
        if task == 'IAMAT' and check != -1:
            await self.flood(response)
        if task != 'AT':
            writer.write(response.encode())
            await writer.drain()
            writer.write_eof()

        #close the connection to the client
        writer.close()

    async def run_forever(self):
        server = await asyncio.start_server(self.handle_input, self.ip, self.port)

        # Serve requests until Ctrl+C is pressed
        async with server:
            await server.serve_forever()
        # Close the server
        server.close()




if __name__ == '__main__':
    parser = argparse.ArgumentParser('Server name and connection to server.')
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()

    server = Server(args.server_name,port=server_ports[args.server_name])
    logging.basicConfig(filename='{0}_server.log'.format(server.name), level=logging.INFO, filemode='w+',format='%(message)s')

    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass

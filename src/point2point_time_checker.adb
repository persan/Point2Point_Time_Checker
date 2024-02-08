pragma Extensions_Allowed (On);
with Ada.Calendar;
with Ada.Text_IO;
with GNAT.Sockets;
with GNATCOLL.Opt_Parse;
with Ada.Streams;
procedure Point2Point_Time_Checker is
   use Ada.Streams;
   use Ada.Calendar;
   use GNAT.Sockets;
   use Ada.Text_IO;

   package Args is
      use GNATCOLL.Opt_Parse;

      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Tool to measure time skew and transport time between two nodes i a network");
      function Convert (S : String) return GNAT.Sockets.Port_Type is
        (GNAT.Sockets.Port_Type'Value (S));

      Default_Port : constant GNAT.Sockets.Port_Type := 8888;

      package Port is new Parse_Option
        (Parser      => Parser,
         Short       => "-p",
         Long        => "--port",
         Arg_Type    => GNAT.Sockets.Port_Type,
         Help        => "Server port """ & Default_Port'Image & """",
         Default_Val => Default_Port);

      function Convert (Host_Name : String) return GNAT.Sockets.Inet_Addr_Type;

      package Remote_Server is new Parse_Option
        (Parser      => Parser,
         Short       => "-h",
         Long        => "--host",
         Arg_Type    => GNAT.Sockets.Inet_Addr_Type,
         Help        => "Server:Server ip. Default is localhost",
         Default_Val => GNAT.Sockets.Loopback_Inet_Addr);

      package Local_Ip is new Parse_Option
        (Parser      => Parser,
         Short       => "-l",
         Long        => "--local",
         Arg_Type    => GNAT.Sockets.Inet_Addr_Type,
         Help        => "Client:Client listen on network. Default is Any_ip",
         Default_Val => GNAT.Sockets.Any_Inet_Addr);

      package Server is new Parse_Flag
        (Parser => Parser,
         Short  => "-s",
         Long   => "--server",
         Help   => "Run in server mode");
   end Args;

   package body Args is
      function Convert (Host_Name : String) return GNAT.Sockets.Inet_Addr_Type is
      begin
         if Is_IPv4_Address (Host_Name) or else Is_IPv6_Address (Host_Name) then
            return Inet_Addr (Host_Name);
         else
            return Addresses (Get_Host_By_Name (Host_Name), 1);
         end if;
      end;
   end Args;

   type Msg_Type is record
      Client_Clock : Ada.Calendar.Time;
      Server_Clock : Ada.Calendar.Time;
   end record;

   type Buffered_Msg_Type (Part : Boolean := False) is record
      case Part is
         when True  => As_Msg_Type              :
            Msg_Type;
         when False => As_Steram_Element_Array  :
            Stream_Element_Array (1 .. Msg_Type'Size / Stream_Element'Size);
      end case;
   end record with Unchecked_Union => True;

   Address             : aliased Sock_Addr_Type;
   Socket              : Socket_Type;
   Buffer              : Buffered_Msg_Type;
   Transport_Time      : Duration;
   Delta_Time          : Duration;
   Last                : Ada.Streams.Stream_Element_Offset;
   Adjusted_Delta_Time : Duration;
begin
   if Args.Parser.Parse then
      Socket.Create_Socket (Family_Inet, Socket_Datagram);
      Socket.Set_Socket_Option (Socket_Level, (Reuse_Address, True));
      Socket.Set_Socket_Option (IP_Protocol_For_IP_Level, (Multicast_TTL, 1));

      Address.Port := Any_Port;
      if Args.Server.Get then

         Address.Addr := Args.Local_Ip.Get;
         Bind_Socket (Socket, Address);
         loop
            Socket.Receive_Socket (Buffer.As_Steram_Element_Array, Last, Address);
            Buffer.As_Msg_Type.Server_Clock := Ada.Calendar.Clock;
            Socket.Send_Socket (Buffer.As_Steram_Element_Array, Last, Address);
         end loop;
      else
         Socket.Set_Socket_Option (Socket_Level, (Receive_Timeout, 0.1));
         Address.Addr := Any_Inet_Addr;
         Bind_Socket (Socket, Address);

         Address.Addr := Args.Remote_Server.Get;
         Address.Port := Args.Port.Get;

         loop
            Buffer.As_Msg_Type.Client_Clock := Ada.Calendar.Clock;

            Socket.Send_Socket (Buffer.As_Steram_Element_Array, Last, Address'Access);
            begin
               Socket.Receive_Socket (Buffer.As_Steram_Element_Array, Last);

               Transport_Time := Ada.Calendar.Clock - Buffer.As_Msg_Type.Client_Clock;
               Delta_Time  := Buffer.As_Msg_Type.Server_Clock - Buffer.As_Msg_Type.Client_Clock;
               Adjusted_Delta_Time := Delta_Time - Transport_Time / 2.0;
               Put_Line (Transport_Time'Image & ", "  & Delta_Time'Image & ", " & Adjusted_Delta_Time'Image);
            exception
               when Socket_Error =>
                  Put_Line ("No response from remote");
            end;
            delay 1.0;
         end loop;
      end if;
   else
      Put_Line  (Args.Parser.Help);
   end if;
end Point2Point_Time_Checker;

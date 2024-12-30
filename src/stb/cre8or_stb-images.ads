------------------------------------------------------------------------------------------------------------------------
--  Copyright 2024 Cre8or                                                                                             --
--                                                                                                                    --
--  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance    --
--  with the License. You may obtain a copy of the License at                                                         --
--                                                                                                                    --
--     http://www.apache.org/licenses/LICENSE-2.0                                                                     --
--                                                                                                                    --
--  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed  --
--  on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.                     --
--  See the License for the specific language governing permissions and limitations under the License.                --
------------------------------------------------------------------------------------------------------------------------





with System;

private with Cre8or_stb.API;



pragma Elaborate_All (System);

pragma Elaborate_All (Cre8or_stb.API);





package Cre8or_stb.Images is



	-- Exceptions
	EX_FILE_READ_ERROR  : exception;
	EX_FILE_WRITE_ERROR : exception;



	-- Types
	subtype T_JPG_Quality is T_Natural range 1 .. 100;



	-- Specifications
	---------------------------------------------------------------------------------------------------------------------
	procedure Load_File_Info (
		File_Path : in     String;
		Width     :    out T_Natural;
		Height    :    out T_Natural;
		Channels  :    out T_Natural
	);

	---------------------------------------------------------------------------------------------------------------------
	function Get_File_Data (File_Path : in String) return T_Data_Chunk;

	---------------------------------------------------------------------------------------------------------------------
	procedure Free_File_Data (Data : in out T_Data_Chunk);

	---------------------------------------------------------------------------------------------------------------------
	procedure Write_BMP (
		File_Path : in String;
		Width     : in T_Natural;
		Height    : in T_Natural;
		Channels  : in T_Natural;
		Data      : in T_Address
	);

	---------------------------------------------------------------------------------------------------------------------
	procedure Write_JPG (
		File_Path : in String;
		Width     : in T_Natural;
		Height    : in T_Natural;
		Channels  : in T_Natural;
		Data      : in T_Address;
		Quality   : in T_JPG_Quality
	);

	---------------------------------------------------------------------------------------------------------------------
	procedure Write_PNG (
		File_Path : in String;
		Width     : in T_Natural;
		Height    : in T_Natural;
		Channels  : in T_Natural;
		Data      : in T_Address
	);

	---------------------------------------------------------------------------------------------------------------------
	procedure Write_TGA (
		File_Path : in String;
		Width     : in T_Natural;
		Height    : in T_Natural;
		Channels  : in T_Natural;
		Data      : in T_Address
	);



private



	-- Specifications
	---------------------------------------------------------------------------------------------------------------------
	procedure Raise_Write_Error;



end Cre8or_stb.Images;

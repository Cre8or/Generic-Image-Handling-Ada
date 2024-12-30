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





with Interfaces.C.Strings;



pragma Elaborate_All (Interfaces.C.Strings);





package body Cre8or_stb.Images is



	-- Use claues
	use Interfaces;

	use type C.Strings.chars_ptr;



	-- Bodies
	---------------------------------------------------------------------------------------------------------------------
	procedure Load_File_Info (
		File_Path : in     String;
		Width     :    out T_Natural;
		Height    :    out T_Natural;
		Channels  :    out T_Natural
	) is

		Chars       : aliased C.char_array         := C.To_C (File_Path);
		Chars_Ptr   : constant Cre8or_stb.API.T_Chars_Ptr := C.Strings.To_Chars_Ptr (Chars'Unchecked_Access);

		LL_Width    : aliased T_Integer;
		LL_Height   : aliased T_Integer;
		LL_Channels : aliased T_Integer;

		Status      : T_Integer;

	begin

		-- Load the image header
		Status := Cre8or_stb.API.stbi_info (
			filename => Chars_Ptr,
			x        => LL_Width'Access,
			y        => LL_Height'Access,
			comp     => LL_Channels'Access
		);

		if Status /= 1 then
			declare
				Error_Message_Ptr : C.Strings.chars_ptr := Cre8or_stb.API.stbi_failure_reason;
			begin

				if Error_Message_Ptr = C.Strings.Null_Ptr then
					raise EX_FILE_READ_ERROR with "unknown error";
				end if;

				raise EX_FILE_READ_ERROR with C.Strings.Value (Error_Message_Ptr);
			end;
		end if;

		Width    := LL_Width;
		Height   := LL_Height;
		Channels := LL_Channels;

	end Load_File_Info;

	---------------------------------------------------------------------------------------------------------------------
	function Get_File_Data (File_Path : in String) return T_Data_Chunk is

		Chars       : aliased C.char_array         := C.To_C (File_Path);
		Chars_Ptr   : constant Cre8or_stb.API.T_Chars_Ptr := C.Strings.To_Chars_Ptr (Chars'Unchecked_Access);

		LL_Width    : aliased T_Integer;
		LL_Height   : aliased T_Integer;
		LL_Channels : aliased T_Integer;

		Result      : T_Data_Chunk;

	begin

		Result.m_Address := Cre8or_stb.API.stbi_load (
			filename         => Chars_Ptr,
			x                => LL_Width'Access,
			y                => LL_Height'Access,
			channels_in_file => LL_Channels'Access,
			desired_channels => 0
		);

		if Result.m_Address = C_Null then
			raise EX_FILE_READ_ERROR;
		end if;

		Result.m_Length := LL_Width * LL_Height * LL_Channels;

		return Result;

	end Get_File_Data;

	---------------------------------------------------------------------------------------------------------------------
	procedure Free_File_Data (Data : in out T_Data_Chunk) is
	begin

		Cre8or_stb.API.stbi_image_free (Data.m_Address);

		Data.m_Address := C_Null;
		Data.m_Length  := 0;

	end Free_File_Data;

	---------------------------------------------------------------------------------------------------------------------
	procedure Write_BMP (
		File_Path : in String;
		Width     : in T_Natural;
		Height    : in T_Natural;
		Channels  : in T_Natural;
		Data      : in T_Address
	) is

		Chars     : aliased C.char_array         := C.To_C (File_Path);
		Chars_Ptr : constant Cre8or_stb.API.T_Chars_Ptr := C.Strings.To_Chars_Ptr (Chars'Unchecked_Access);
		Status    : T_Integer;

	begin

		Status := Cre8or_stb.API.stbi_write_bmp (
			filename => Chars_Ptr,
			w        => Width,
			h        => Height,
			comp     => Channels,
			data     => Data
		);

		if Status /= 1 then
			Raise_Write_Error;
		end if;

	end Write_BMP;

	---------------------------------------------------------------------------------------------------------------------
	procedure Write_JPG (
		File_Path : in String;
		Width     : in T_Natural;
		Height    : in T_Natural;
		Channels  : in T_Natural;
		Data      : in T_Address;
		Quality   : in T_JPG_Quality
	) is

		Chars     : aliased C.char_array         := C.To_C (File_Path);
		Chars_Ptr : constant Cre8or_stb.API.T_Chars_Ptr := C.Strings.To_Chars_Ptr (Chars'Unchecked_Access);
		Status    : T_Integer;

	begin

		Status := Cre8or_stb.API.stbi_write_jpg (
			filename => Chars_Ptr,
			w        => Width,
			h        => Height,
			comp     => Channels,
			data     => Data,
			quality  => Quality
		);

		if Status /= 1 then
			Raise_Write_Error;
		end if;

	end Write_JPG;

	---------------------------------------------------------------------------------------------------------------------
	procedure Write_PNG (
		File_Path : in String;
		Width     : in T_Natural;
		Height    : in T_Natural;
		Channels  : in T_Natural;
		Data      : in T_Address
	) is

		Chars     : aliased C.char_array         := C.To_C (File_Path);
		Chars_Ptr : constant Cre8or_stb.API.T_Chars_Ptr := C.Strings.To_Chars_Ptr (Chars'Unchecked_Access);
		Status    : T_Integer;

	begin

		Status := Cre8or_stb.API.stbi_write_png (
			filename        => Chars_Ptr,
			w               => Width,
			h               => Height,
			comp            => Channels,
			data            => Data,
			stride_in_bytes => Width * Channels
		);

		if Status /= 1 then
			Raise_Write_Error;
		end if;

	end Write_PNG;


	---------------------------------------------------------------------------------------------------------------------
	procedure Write_TGA (
		File_Path : in String;
		Width     : in T_Natural;
		Height    : in T_Natural;
		Channels  : in T_Natural;
		Data      : in T_Address
	) is

		Chars     : aliased C.char_array         := C.To_C (File_Path);
		Chars_Ptr : constant Cre8or_stb.API.T_Chars_Ptr := C.Strings.To_Chars_Ptr (Chars'Unchecked_Access);
		Status    : T_Integer;

	begin

		Status := Cre8or_stb.API.stbi_write_tga (
			filename => Chars_Ptr,
			w        => Width,
			h        => Height,
			comp     => Channels,
			data     => Data
		);

		if Status /= 1 then
			Raise_Write_Error;
		end if;

	end Write_TGA;



-- PRIVATE



	---------------------------------------------------------------------------------------------------------------------
	procedure Raise_Write_Error is

		Error_Message_Ptr : constant C.Strings.chars_ptr := Cre8or_stb.API.stbi_failure_reason;

	begin

		if Error_Message_Ptr = C.Strings.Null_Ptr then
			raise EX_FILE_WRITE_ERROR;
		end if;

		raise EX_FILE_WRITE_ERROR with C.Strings.Value (Error_Message_Ptr);

	end Raise_Write_Error;



end Cre8or_stb.Images;

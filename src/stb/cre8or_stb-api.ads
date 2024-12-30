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





private package Cre8or_stb.API is



	-- Types
	subtype T_Chars_Ptr is Interfaces.C.Strings.chars_ptr;



	-- Specifications
	---------------------------------------------------------------------------------------------------------------------
	-- std_image.h
	---------------------------------------------------------------------------------------------------------------------
	function stbi_failure_reason return T_Chars_Ptr
	with Import, Convention => C, External_Name => "stbi_failure_reason";

	---------------------------------------------------------------------------------------------------------------------
	procedure stbi_image_free (retval_from_stbi_load : T_Address)
	with Import, Convention => C, External_Name => "stbi_image_free";

	---------------------------------------------------------------------------------------------------------------------
	function stbi_info (
		filename : T_Chars_Ptr;
		x        : not null access T_Integer;
		y        : not null access T_Integer;
		comp     : not null access T_Integer
	) return T_Integer
	with Import, Convention => C, External_Name => "stbi_info";

	---------------------------------------------------------------------------------------------------------------------
	 function stbi_load (
	 	filename         : T_Chars_Ptr;
	 	x                : not null access T_Integer;
	 	y                : not null access T_Integer;
	 	channels_in_file : not null access T_Integer;
	 	desired_channels : T_Integer
	) return T_Address
	with Import, Convention => C, External_Name => "stbi_load";



	---------------------------------------------------------------------------------------------------------------------
	-- std_image_writer.h
	---------------------------------------------------------------------------------------------------------------------
	function stbi_write_png (
		filename        : T_Chars_Ptr;
		w               : T_Integer;
		h               : T_Integer;
		comp            : T_Integer;
		data            : T_Address;
		stride_in_bytes : T_Integer
	) return T_Integer
	with Import, Convention => C, External_Name => "stbi_write_png";

	---------------------------------------------------------------------------------------------------------------------
	function stbi_write_bmp (
		filename : T_Chars_Ptr;
		w        : T_Integer;
		h        : T_Integer;
		comp     : T_Integer;
		data     : T_Address
	) return T_Integer
	with Import, Convention => C, External_Name => "stbi_write_bmp";

	---------------------------------------------------------------------------------------------------------------------
	function stbi_write_tga (
		filename : T_Chars_Ptr;
		w        : T_Integer;
		h        : T_Integer;
		comp     : T_Integer;
		data     : T_Address
	) return T_Integer
	with Import, Convention => C, External_Name => "stbi_write_tga";

	---------------------------------------------------------------------------------------------------------------------
	function stbi_write_jpg (
		filename : T_Chars_Ptr;
		w        : T_Integer;
		h        : T_Integer;
		comp     : T_Integer;
		data     : T_Address;
		quality  : T_Integer
	) return T_Integer
	with Import, Convention => C, External_Name => "stbi_write_jpg";



end Cre8or_stb.API;

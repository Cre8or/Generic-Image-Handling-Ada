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





with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;



pragma Elaborate_All (Ada.Characters.Handling);
pragma Elaborate_All (Ada.Directories);
pragma Elaborate_All (Ada.Exceptions);
pragma Elaborate_All (Ada.Strings.Fixed);





package body Cre8or_Generic_Image_Handling is



	-- Generics
	---------------------------------------------------------------------------------------------------------------------
	function Gen_Parse_Raw_Data (
		Raw_Data : in Cre8or_stb.T_Data_Chunk;
		Width    : in T_Size;
		Height   : in T_Size
	) return T_Gen_Colour_Arr_Ref is

		Source_Buffer       : T_Byte_Arr (1 .. Raw_Data.m_Length);
		for Source_Buffer'Address use Raw_Data.m_Address;

		Result              : T_Gen_Colour_Arr_Ref := new T_Gen_Colour_Arr (1 .. Height, 1 .. Width);
		Result_Buffer       : T_Byte_Arr (1 .. T_Natural (Width) * T_Natural (Height) * (T_Gen_Colour'Size / 8));
		for Result_Buffer'Address use Result.all'Address;

	begin

		Result_Buffer := Source_Buffer;

		return Result;

	end Gen_Parse_Raw_Data;

	---------------------------------------------------------------------------------------------------------------------
	function Gen_Convert_Raw_Data (
		Raw_Data        : in Cre8or_stb.T_Data_Chunk;
		Width           : in T_Size;
		Height          : in T_Size;
		Source_Channels : in T_Natural;
		Target_Channels : in T_Natural;
		Fill_Value      : in T_Byte
	) return T_Gen_Colour_Arr_Ref is

		C_Source_Length     : constant T_Natural := Raw_Data.m_Length;
		C_Max_Source_Offset : constant T_Natural := Source_Channels - 1;
		C_Max_Target_Offset : constant T_Natural := Target_Channels - 1;

		Source_Buffer       : T_Byte_Arr (1 .. C_Source_Length);
		for Source_Buffer'Address use Raw_Data.m_Address;

		Result              : T_Gen_Colour_Arr_Ref := new T_Gen_Colour_Arr (1 .. Height, 1 .. Width);
		Result_Buffer       : T_Byte_Arr (1 .. T_Natural (Width) * T_Natural (Height) * Target_Channels);
		for Result_Buffer'Address use Result.all'Address;

		Source_Index        : T_Natural := 1;
		Target_Index        : T_Natural := 1;

	begin

		-- Copy the existing data and fill the gaps
		if Source_Channels < Target_Channels then

			while Source_Index < C_Source_Length loop

				-- Existing data
				Result_Buffer (Target_Index .. Target_Index + C_Max_Source_Offset) := Source_Buffer (Source_Index .. Source_Index + C_Max_Source_Offset);

				-- Missing (new) data
				Result_Buffer (Target_Index + Source_Channels .. Target_Index + C_Max_Target_Offset) := (others => Fill_Value);

				Source_Index := Source_Index + Source_Channels;
				Target_Index := Target_Index + Target_Channels;

			end loop;

		-- Copy a susbet of the data and discard the rest
		else

			while Source_Index < C_Source_Length loop

				Result_Buffer (Target_Index .. Target_Index + C_Max_Target_Offset) := Source_Buffer (Source_Index .. Source_Index + C_Max_Target_Offset);

				Source_Index := Source_Index + Source_Channels;
				Target_Index := Target_Index + Target_Channels;

			end loop;

		end if;

		return Result;

	end Gen_Convert_Raw_Data;

	---------------------------------------------------------------------------------------------------------------------
	procedure Gen_Flip_Horizontally (Data : in out T_Gen_Colour_Arr) is

		Colour : T_Gen_Colour;
		Last   : constant T_Size := Data'Last (2);
		Offset : constant T_Size'Base := Last + 1; -- Arrays are expected to start at 1

	begin

		for Y in Data'Range (1) loop

			for X in 1 .. Last / 2 loop

				Colour                := Data (Y, Offset - X);
				Data (Y, Offset - X) := Data (Y, X);
				Data (Y, X)          := Colour;

			end loop;

		end loop;

	end Gen_Flip_Horizontally;

	---------------------------------------------------------------------------------------------------------------------
	procedure Gen_Flip_Vertically (Data : in out T_Gen_Colour_Arr) is

		Colour : T_Gen_Colour;
		Last   : constant T_Size      := Data'Last (1);
		Offset : constant T_Size'Base := Last + 1; -- Arrays are expected to start at 1

	begin

  		for Y in 1 .. Last / 2 loop

  			for X in Data'Range (2) loop

  				Colour               := Data (Offset - Y, X);
				Data (Offset - Y, X) := Data (Y, X);
				Data (Y, X)          := Colour;

			end loop;

		end loop;

	end Gen_Flip_Vertically;

	---------------------------------------------------------------------------------------------------------------------
	procedure Gen_Rotate_CW (Data : in out T_Gen_Colour_Arr_Ref) is

		Data_New : T_Gen_Colour_Arr_Ref := new T_Gen_Colour_Arr (Data'Range (2), Data'Range (1)); -- Swap the ranges

		First  : constant T_Size := Data'First (1);
		Last   : constant T_Size := Data'Last (1);
		Offset : constant T_Size'Base := First + Last;

	begin

		for Y in Data'Range (1) loop

			for X in Data'Range (2) loop

				Data_New (X, Offset - Y) := Data (Y, X);

			end loop;

		end loop;

		Deallocate (Data);
		Data := Data_New;

	end Gen_Rotate_CW;

	---------------------------------------------------------------------------------------------------------------------
	procedure Gen_Rotate_CCW (Data : in out T_Gen_Colour_Arr_Ref) is

		Data_New : T_Gen_Colour_Arr_Ref := new T_Gen_Colour_Arr (Data'Range (2), Data'Range (1)); -- Swap the ranges

		First  : constant T_Size := Data'First (2);
		Last   : constant T_Size := Data'Last (2);
		Offset : constant T_Size'Base := First + Last;

	begin

		for Y in Data'Range (1) loop

			for X in Data'Range (2) loop

				Data_New (Offset - X, Y) := Data (Y, X);

			end loop;

		end loop;

		Deallocate (Data);
		Data := Data_New;

	end Gen_Rotate_CCW;

	---------------------------------------------------------------------------------------------------------------------
	procedure Gen_Rotate_180 (Data : in out T_Gen_Colour_Arr) is

		Copy : constant T_Gen_Colour_Arr := Data;

		First_X  : constant T_Size := Data'First (2);
		Last_X   : constant T_Size := Data'Last (2);
		Offset_X : constant T_Size'Base := First_X + Last_X;

		First_Y  : constant T_Size := Data'First (1);
		Last_Y   : constant T_Size := Data'Last (1);
		Offset_Y : constant T_Size'Base := First_Y + Last_Y;

	begin

		for Y in Copy'Range (1) loop

			for X in Copy'Range (2) loop

				Data (Offset_Y - Y, Offset_X - X) := Copy (Y, X);

			end loop;

		end loop;

	end Gen_Rotate_180;

	---------------------------------------------------------------------------------------------------------------------
	procedure Gen_Insert (
		Source   : in     T_Gen_Colour_Arr;
		Target   : in out T_Gen_Colour_Arr;
		X_Insert : in     T_Size;
		Y_Insert : in     T_Size;
		Region   : in     T_Region
	) is

		X_First : constant T_Size'Base := T_Size'Max (X_Insert, Target'First (2));
		Y_First : constant T_Size'Base := T_Size'Max (Y_Insert, Target'First (1));

		X_Last : constant T_Size'Base := T_Size'Min (Target'Last (2), X_Insert + Region.Right - Region.Left);
		Y_Last : constant T_Size'Base := T_Size'Min (Target'Last (1), Y_Insert + Region.Bottom - Region.Top);

		Offset_X : constant T_Size'Base := Region.Left - X_Insert;
		Offset_Y : constant T_Size'Base := Region.Top - Y_Insert;

	begin

		for Y in Y_First .. Y_Last loop

			for X in X_First .. X_Last loop

				Target (Y, X) := Source (Y + Offset_Y, X + Offset_X);

			end loop;

		end loop;

	end Gen_Insert;



	-- Instantiations
	procedure Deallocate is new Ada.Unchecked_Deallocation (T_Data_Grey,  T_Data_Grey_Ref);
	procedure Deallocate is new Ada.Unchecked_Deallocation (T_Data_GreyA, T_Data_GreyA_Ref);
	procedure Deallocate is new Ada.Unchecked_Deallocation (T_Data_RGB,   T_Data_RGB_Ref);
	procedure Deallocate is new Ada.Unchecked_Deallocation (T_Data_RGBA,  T_Data_RGBA_Ref);

	procedure Deallocate is new Ada.Unchecked_Deallocation (T_Colour_Grey_Arr,  T_Colour_Grey_Arr_Ref);
	procedure Deallocate is new Ada.Unchecked_Deallocation (T_Colour_GreyA_Arr, T_Colour_GreyA_Arr_Ref);
	procedure Deallocate is new Ada.Unchecked_Deallocation (T_Colour_RGB_Arr,   T_Colour_RGB_Arr_Ref);
	procedure Deallocate is new Ada.Unchecked_Deallocation (T_Colour_RGBA_Arr,  T_Colour_RGBA_Arr_Ref);

	function Parse_Grey  is new Gen_Parse_Raw_Data (T_Byte,         T_Colour_Grey_Arr,  T_Colour_Grey_Arr_Ref);
	function Parse_GreyA is new Gen_Parse_Raw_Data (T_Colour_GreyA, T_Colour_GreyA_Arr, T_Colour_GreyA_Arr_Ref);
	function Parse_RGB   is new Gen_Parse_Raw_Data (T_Colour_RGB,   T_Colour_RGB_Arr,   T_Colour_RGB_Arr_Ref);
	function Parse_RGBA  is new Gen_Parse_Raw_Data (T_Colour_RGBA,  T_Colour_RGBA_Arr,  T_Colour_RGBA_Arr_Ref);

	function Convert_To_Grey  is new Gen_Convert_Raw_Data (T_Byte,         T_Colour_Grey_Arr,  T_Colour_Grey_Arr_Ref);
	function Convert_To_GreyA is new Gen_Convert_Raw_Data (T_Colour_GreyA, T_Colour_GreyA_Arr, T_Colour_GreyA_Arr_Ref);
	function Convert_To_RGB   is new Gen_Convert_Raw_Data (T_Colour_RGB,   T_Colour_RGB_Arr,   T_Colour_RGB_Arr_Ref);
	function Convert_To_RGBA  is new Gen_Convert_Raw_Data (T_Colour_RGBA,  T_Colour_RGBA_Arr,  T_Colour_RGBA_Arr_Ref);

	procedure Flip_Horizontally_Grey  is new Gen_Flip_Horizontally (T_Byte,         T_Colour_Grey_Arr);
	procedure Flip_Horizontally_GreyA is new Gen_Flip_Horizontally (T_Colour_GreyA, T_Colour_GreyA_Arr);
	procedure Flip_Horizontally_RGB   is new Gen_Flip_Horizontally (T_Colour_RGB,   T_Colour_RGB_Arr);
	procedure Flip_Horizontally_RGBA  is new Gen_Flip_Horizontally (T_Colour_RGBA,  T_Colour_RGBA_Arr);

	procedure Flip_Vertically_Grey  is new Gen_Flip_Vertically (T_Byte,         T_Colour_Grey_Arr);
	procedure Flip_Vertically_GreyA is new Gen_Flip_Vertically (T_Colour_GreyA, T_Colour_GreyA_Arr);
	procedure Flip_Vertically_RGB   is new Gen_Flip_Vertically (T_Colour_RGB,   T_Colour_RGB_Arr);
	procedure Flip_Vertically_RGBA  is new Gen_Flip_Vertically (T_Colour_RGBA,  T_Colour_RGBA_Arr);

	procedure Rotate_CW_Grey  is new Gen_Rotate_CW (T_Byte,         T_Colour_Grey_Arr,  T_Colour_Grey_Arr_Ref,  Deallocate);
	procedure Rotate_CW_GreyA is new Gen_Rotate_CW (T_Colour_GreyA, T_Colour_GreyA_Arr, T_Colour_GreyA_Arr_Ref, Deallocate);
	procedure Rotate_CW_RGB   is new Gen_Rotate_CW (T_Colour_RGB,   T_Colour_RGB_Arr,   T_Colour_RGB_Arr_Ref,   Deallocate);
	procedure Rotate_CW_RGBA  is new Gen_Rotate_CW (T_Colour_RGBA,  T_Colour_RGBA_Arr,  T_Colour_RGBA_Arr_Ref,  Deallocate);

	procedure Rotate_CCW_Grey  is new Gen_Rotate_CCW (T_Byte,         T_Colour_Grey_Arr,  T_Colour_Grey_Arr_Ref,  Deallocate);
	procedure Rotate_CCW_GreyA is new Gen_Rotate_CCW (T_Colour_GreyA, T_Colour_GreyA_Arr, T_Colour_GreyA_Arr_Ref, Deallocate);
	procedure Rotate_CCW_RGB   is new Gen_Rotate_CCW (T_Colour_RGB,   T_Colour_RGB_Arr,   T_Colour_RGB_Arr_Ref,   Deallocate);
	procedure Rotate_CCW_RGBA  is new Gen_Rotate_CCW (T_Colour_RGBA,  T_Colour_RGBA_Arr,  T_Colour_RGBA_Arr_Ref,  Deallocate);

	procedure Rotate_180_Grey  is new Gen_Rotate_180 (T_Byte,         T_Colour_Grey_Arr);
	procedure Rotate_180_GreyA is new Gen_Rotate_180 (T_Colour_GreyA, T_Colour_GreyA_Arr);
	procedure Rotate_180_RGB   is new Gen_Rotate_180 (T_Colour_RGB,   T_Colour_RGB_Arr);
	procedure Rotate_180_RGBA  is new Gen_Rotate_180 (T_Colour_RGBA,  T_Colour_RGBA_Arr);

	procedure Insert_Grey  is new Gen_Insert (T_Byte,         T_Colour_Grey_Arr);
	procedure Insert_GreyA is new Gen_Insert (T_Colour_GreyA, T_Colour_GreyA_Arr);
	procedure Insert_RGB   is new Gen_Insert (T_Colour_RGB,   T_Colour_RGB_Arr);
	procedure Insert_RGBA  is new Gen_Insert (T_Colour_RGBA,  T_Colour_RGBA_Arr);



	-- Primitives
	---------------------------------------------------------------------------------------------------------------------
	-- T_Handle
	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Create (
		Handle        : in out T_Image;
		Width, Height : in     T_Size;
		Colour_Format : in     T_Colour_Format;
		Default_Value : in     T_Byte := 255
	) is
	begin

		Handle.Finalize;

		Handle.m_Colour_Format := Colour_Format;

		case Colour_Format is

			when E_Grey  => Handle.m_Data_Grey  := new T_Data_Grey'(
				Width     => Width,
				Height    => Height,
				Pixels    => new T_Colour_Grey_Arr'(1 .. Height => (1 .. Width => Default_Value)),
				Ref_Count => <> -- Default value is one
			);

			when E_GreyA => Handle.m_Data_GreyA := new T_Data_GreyA'(
				Width     => Width,
				Height    => Height,
				Pixels    => new T_Colour_GreyA_Arr'(1 .. Height => (1 .. Width => (others => Default_Value))),
				Ref_Count => <> -- Default value is one
			);

			when E_RGB   => Handle.m_Data_RGB   := new T_Data_RGB'(
				Width     => Width,
				Height    => Height,
				Pixels    => new T_Colour_RGB_Arr'(1 .. Height => (1 .. Width => (others => Default_Value))),
				Ref_Count => <> -- Default value is one
			);

			when E_RGBA  => Handle.m_Data_RGBA  := new T_Data_RGBA'(
				Width     => Width,
				Height    => Height,
				Pixels    => new T_Colour_RGBA_Arr'(1 .. Height => (1 .. Width => (others => Default_Value))),
				Ref_Count => <> -- Default value is one
			);

			when E_Undefined => raise EX_WRONG_COLOUR_FORMAT;

		end case;

	end Create;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Create_From (
		Handle : in out T_Image;
		Data   : in     T_Colour_Grey_Arr
	) is
	begin

		if Data'First (1) /= 1 or Data'First (2) /= 1 then
			raise EX_INDEX_OUT_OF_BOUNDS with "Colour data must start at index 1 (on both dimensions)";
		end if;

		Handle.Finalize;

		Handle.m_Colour_Format := E_Grey;
		Handle.m_Data_Grey     := new T_Data_Grey'(
			Width     => Data'Length (2),
			Height    => Data'Length (1),
			Pixels    => new T_Colour_Grey_Arr'(Data),
			Ref_Count => <> -- Default value is one
		);

	end Create_From;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Create_From (
		Handle : in out T_Image;
		Data   : in     T_Colour_GreyA_Arr
	) is
	begin

		if Data'First (1) /= 1 or Data'First (2) /= 1 then
			raise EX_INDEX_OUT_OF_BOUNDS with "Colour data must start at index 1 (on both dimensions)";
		end if;

		Handle.Finalize;

		Handle.m_Colour_Format := E_GreyA;
		Handle.m_Data_GreyA    := new T_Data_GreyA'(
			Width     => Data'Length (2),
			Height    => Data'Length (1),
			Pixels    => new T_Colour_GreyA_Arr'(Data),
			Ref_Count => <> -- Default value is one
		);

	end Create_From;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Create_From (
		Handle : in out T_Image;
		Data   : in     T_Colour_RGB_Arr
	) is
	begin

		if Data'First (1) /= 1 or Data'First (2) /= 1 then
			raise EX_INDEX_OUT_OF_BOUNDS with "Colour data must start at index 1 (on both dimensions)";
		end if;

		Handle.Finalize;

		Handle.m_Colour_Format := E_RGB;
		Handle.m_Data_RGB      := new T_Data_RGB'(
			Width     => Data'Length (2),
			Height    => Data'Length (1),
			Pixels    => new T_Colour_RGB_Arr'(Data),
			Ref_Count => <> -- Default value is one
		);

	end Create_From;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Create_From (
		Handle : in out T_Image;
		Data   : in     T_Colour_RGBA_Arr
	) is
	begin

		if Data'First (1) /= 1 or Data'First (2) /= 1 then
			raise EX_INDEX_OUT_OF_BOUNDS with "Colour data must start at index 1 (on both dimensions)";
		end if;

		Handle.Finalize;

		Handle.m_Colour_Format := E_RGBA;
		Handle.m_Data_RGBA     := new T_Data_RGBA'(
			Width     => Data'Length (2),
			Height    => Data'Length (1),
			Pixels    => new T_Colour_RGBA_Arr'(Data),
			Ref_Count => <> -- Default value is one
		);

	end Create_From;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Convert_To (
		Handle        : in out T_Image;
		Colour_Format : in     T_Colour_Format;
		Default_Value : in     T_Byte := 255
	) is
	begin

		Handle.Convert_To (Handle, Colour_Format, Default_Value);

	end Convert_To;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Convert_To (
		Handle        : in     T_Image;
		Target        :    out T_Image'Class;
		Colour_Format : in     T_Colour_Format;
		Default_Value : in     T_Byte := 255
	) is
	begin

		-- Handle edge cases
		if Colour_Format = Handle.m_Colour_Format then
			Handle.Copy_To (Target);
			return;

		elsif Colour_Format = E_Undefined then
			Target.Finalize;
			return;

		elsif Handle.m_Colour_Format = E_Undefined then
			raise EX_INVALID_HANDLE;
		end if;

		-- Make it harder to accidentally cause unsafe conversions
		pragma Compile_Time_Warning (
			T_Colour_Format'Pos (T_Colour_Format'Last) > 4,
			"Unhandled cases in conversion of T_Colour_Format!" & ASCII.LF &
			"The following conversion to integer may raise CONSTRAINT_ERROR"
		);

		declare

			Width           : T_Size    := Handle.Get_Width;
			Height          : T_Size    := Handle.Get_Height;
			Source_Channels : T_Natural := T_Colour_Format'Pos (Handle.m_Colour_Format);
			Target_Channels : T_Natural := T_Colour_Format'Pos (Colour_Format);

			Data_Chunk      : Cre8or_stb.T_Data_Chunk := (
				m_Address => Handle.Get_Data_Address,
				m_Length  => Handle.Get_Data_Length
			);

			Old_Data_Grey  : T_Data_Grey_Ref  := Handle.m_Data_Grey;
			Old_Data_GreyA : T_Data_GreyA_Ref := Handle.m_Data_GreyA;
			Old_Data_RGB   : T_Data_RGB_Ref   := Handle.m_Data_RGB;
			Old_Data_RGBA  : T_Data_RGBA_Ref  := Handle.m_Data_RGBA;

		begin

			-- Create the new data object
			case Colour_Format is

				when E_Grey  =>
					Target.m_Data_Grey  := new T_Data_Grey'(
						Width     => Width,
						Height    => Height,
						Pixels    => Convert_To_Grey (
							Data_Chunk,
							Width,
							Height,
							Source_Channels,
							Target_Channels,
							Default_Value
						),
						Ref_Count => <> -- Default value is one
					);

				when E_GreyA =>
					Target.m_Data_GreyA := new T_Data_GreyA'(
						Width     => Width,
						Height    => Height,
						Pixels    => Convert_To_GreyA (
							Data_Chunk,
							Width,
							Height,
							Source_Channels,
							Target_Channels,
							Default_Value
						),
						Ref_Count => <> -- Default value is one
					);

				when E_RGB   =>
					Target.m_Data_RGB   := new T_Data_RGB'(
						Width     => Width,
						Height    => Height,
						Pixels    => Convert_To_RGB (
							Data_Chunk,
							Width,
							Height,
							Source_Channels,
							Target_Channels,
							Default_Value
						),
						Ref_Count => <> -- Default value is one
					);

				when E_RGBA  =>
					Target.m_Data_RGBA  := new T_Data_RGBA'(
						Width     => Width,
						Height    => Height,
						Pixels    => Convert_To_RGBA (
							Data_Chunk,
							Width,
							Height,
							Source_Channels,
							Target_Channels,
							Default_Value
						),
						Ref_Count => <> -- Default value is one
					);

				when E_Undefined => null;

			end case;

			-- Edge case: if the source and target handles are the same,
			-- manually drop the reference of the source data.
			case Handle.m_Colour_Format is

				when E_Grey  => Drop_Reference (Old_Data_Grey);
				when E_GreyA => Drop_Reference (Old_Data_GreyA);
				when E_RGB   => Drop_Reference (Old_Data_RGB);
				when E_RGBA  => Drop_Reference (Old_Data_RGBA);

				when E_Undefined => null;

			end case;

			Target.m_Colour_Format := Colour_Format;

		end;

	end Convert_To;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Is_Initialised (Handle : in T_Image) return Boolean is
	begin

		return Handle.m_Colour_Format /= E_Undefined;

	end Is_Initialised;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Load (
		Handle    : in out T_Image;
		File_Path : in     String
	) is

		Sanitised_Path : constant String := Sanitise_File_Path (File_Path);

		LL_Width    : T_Natural;
		LL_Height   : T_Natural;
		LL_Channels : T_Natural;

		Width  : T_Size;
		Height : T_Size;

		Raw_Data : Cre8or_stb.T_Data_Chunk;

	begin

		begin
			Cre8or_stb.Images.Load_File_Info ( Sanitised_Path, LL_Width, LL_Height, LL_Channels);

		exception
			when Ex : Cre8or_stb.Images.EX_FILE_READ_ERROR =>
				raise EX_FILE_READ_ERROR with "STB error: " & Ada.Exceptions.Exception_Message (Ex);
		end;

		if LL_Channels not in 1 .. 4 then
			raise EX_FILE_READ_ERROR with "Incorrect channels count";
		end if;

		Handle.Finalize;

		Width  := T_Size (LL_Width);
		Height := T_Size (LL_Height);
		Handle.m_Colour_Format := T_Colour_Format'Val (LL_Channels);

		Raw_Data := Cre8or_stb.Images.Get_File_Data (Sanitised_Path);

		case Handle.m_Colour_Format is

			when E_Grey  =>
				Handle.m_Data_Grey  := new T_Data_Grey'(
					Width     => Width,
					Height    => Height,
					Pixels    => Parse_Grey (Raw_Data, Width, Height),
					Ref_Count => <> -- Default value is one
				);

			when E_GreyA =>
				Handle.m_Data_GreyA := new T_Data_GreyA'(
					Width     => Width,
					Height    => Height,
					Pixels    => Parse_GreyA (Raw_Data, Width, Height),
					Ref_Count => <> -- Default value is one
				);

			when E_RGB   =>
				Handle.m_Data_RGB   := new T_Data_RGB'(
					Width     => Width,
					Height    => Height,
					Pixels    => Parse_RGB (Raw_Data, Width, Height),
					Ref_Count => <> -- Default value is one
				);

			when E_RGBA   =>
				Handle.m_Data_RGBA  := new T_Data_RGBA'(
					Width     => Width,
					Height    => Height,
					Pixels    => Parse_RGBA (Raw_Data, Width, Height),
					Ref_Count => <> -- Default value is one
				);

			when E_Undefined => raise EX_INVALID_FILE_FORMAT with "Unknown file format";

		end case;

		Cre8or_stb.Images.Free_File_Data (Raw_Data);

	end Load;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Copy_To (
		Handle : in     T_Image;
		Target :    out T_Image'Class
	) is

		Old_Data_Grey  : T_Data_Grey_Ref  := Handle.m_Data_Grey;
		Old_Data_GreyA : T_Data_GreyA_Ref := Handle.m_Data_GreyA;
		Old_Data_RGB   : T_Data_RGB_Ref   := Handle.m_Data_RGB;
		Old_Data_RGBA  : T_Data_RGBA_Ref  := Handle.m_Data_RGBA;

	begin

		if Handle.m_Colour_Format = E_Undefined then
			Target.Finalize;
			return;
		end if;

		Target.m_Colour_Format := Handle.m_Colour_Format;

		case Handle.m_Colour_Format is

			when E_Grey  =>
				Target.m_Data_Grey  := new T_Data_Grey'(
					Width     => Old_Data_Grey.Width,
					Height    => Old_Data_Grey.Height,
					Pixels    => new T_Colour_Grey_Arr'(Old_Data_Grey.Pixels.all),
					Ref_Count => <> -- Default value is one
				);

			when E_GreyA =>
				Target.m_Data_GreyA := new T_Data_GreyA'(
					Width     => Old_Data_GreyA.Width,
					Height    => Old_Data_GreyA.Height,
					Pixels    => new T_Colour_GreyA_Arr'(Old_Data_GreyA.Pixels.all),
					Ref_Count => <> -- Default value is one
				);

			when E_RGB   =>
				Target.m_Data_RGB   := new T_Data_RGB'(
					Width     => Old_Data_RGB.Width,
					Height    => Old_Data_RGB.Height,
					Pixels    => new T_Colour_RGB_Arr'(Old_Data_RGB.Pixels.all),
					Ref_Count => <> -- Default value is one
				);

			when E_RGBA  =>
				Target.m_Data_RGBA  := new T_Data_RGBA'(
					Width     => Old_Data_RGBA.Width,
					Height    => Old_Data_RGBA.Height,
					Pixels    => new T_Colour_RGBA_Arr'(Old_Data_RGBA.Pixels.all),
					Ref_Count => <> -- Default value is one
				);

			when E_Undefined => null;

		end case;

	end Copy_To;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Flip_Horizontally (Handle : in out T_Image) is
	begin

		case Handle.m_Colour_Format is

			when E_Grey  => Flip_Horizontally_Grey (Handle.m_Data_Grey.Pixels.all);
			when E_GreyA => Flip_Horizontally_GreyA (Handle.m_Data_GreyA.Pixels.all);
			when E_RGB   => Flip_Horizontally_RGB (Handle.m_Data_RGB.Pixels.all);
			when E_RGBA  => Flip_Horizontally_RGBA (Handle.m_Data_RGBA.Pixels.all);

			when E_Undefined => raise EX_INVALID_HANDLE;

		end case;

	end Flip_Horizontally;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Flip_Vertically (Handle : in out T_Image) is
	begin

		case Handle.m_Colour_Format is

			when E_Grey  => Flip_Vertically_Grey (Handle.m_Data_Grey.Pixels.all);
			when E_GreyA => Flip_Vertically_GreyA (Handle.m_Data_GreyA.Pixels.all);
			when E_RGB   => Flip_Vertically_RGB (Handle.m_Data_RGB.Pixels.all);
			when E_RGBA  => Flip_Vertically_RGBA (Handle.m_Data_RGBA.Pixels.all);

			when E_Undefined => raise EX_INVALID_HANDLE;

		end case;

	end Flip_Vertically;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Rotate_CW (Handle : in out T_Image) is

		Width : T_Size;

	begin

		case Handle.m_Colour_Format is

			when E_Grey  =>
				Rotate_CW_Grey (Handle.m_Data_Grey.Pixels);

				Width                     := Handle.m_Data_Grey.Width;
				Handle.m_Data_Grey.Width  := Handle.m_Data_Grey.Height;
				Handle.m_Data_Grey.Height := Width;

			when E_GreyA =>
				Rotate_CW_GreyA (Handle.m_Data_GreyA.Pixels);

				Width                      := Handle.m_Data_GreyA.Width;
				Handle.m_Data_GreyA.Width  := Handle.m_Data_GreyA.Height;
				Handle.m_Data_GreyA.Height := Width;

			when E_RGB   =>
				Rotate_CW_RGB (Handle.m_Data_RGB.Pixels);

				Width                    := Handle.m_Data_RGB.Width;
				Handle.m_Data_RGB.Width  := Handle.m_Data_RGB.Height;
				Handle.m_Data_RGB.Height := Width;

			when E_RGBA  =>
				Rotate_CW_RGBA (Handle.m_Data_RGBA.Pixels);

				Width                     := Handle.m_Data_RGBA.Width;
				Handle.m_Data_RGBA.Width  := Handle.m_Data_RGBA.Height;
				Handle.m_Data_RGBA.Height := Width;

			when E_Undefined => raise EX_INVALID_HANDLE;

		end case;

	end Rotate_CW;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Rotate_CCW (Handle : in out T_Image) is
		Width : T_Size;

	begin

		case Handle.m_Colour_Format is

			when E_Grey  =>
				Rotate_CCW_Grey (Handle.m_Data_Grey.Pixels);

				Width                     := Handle.m_Data_Grey.Width;
				Handle.m_Data_Grey.Width  := Handle.m_Data_Grey.Height;
				Handle.m_Data_Grey.Height := Width;

			when E_GreyA =>
				Rotate_CCW_GreyA (Handle.m_Data_GreyA.Pixels);

				Width                      := Handle.m_Data_GreyA.Width;
				Handle.m_Data_GreyA.Width  := Handle.m_Data_GreyA.Height;
				Handle.m_Data_GreyA.Height := Width;

			when E_RGB   =>
				Rotate_CCW_RGB (Handle.m_Data_RGB.Pixels);

				Width                    := Handle.m_Data_RGB.Width;
				Handle.m_Data_RGB.Width  := Handle.m_Data_RGB.Height;
				Handle.m_Data_RGB.Height := Width;

			when E_RGBA  =>
				Rotate_CCW_RGBA (Handle.m_Data_RGBA.Pixels);

				Width                     := Handle.m_Data_RGBA.Width;
				Handle.m_Data_RGBA.Width  := Handle.m_Data_RGBA.Height;
				Handle.m_Data_RGBA.Height := Width;

			when E_Undefined => raise EX_INVALID_HANDLE;

		end case;

	end Rotate_CCW;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Rotate_180 (Handle : in out T_Image) is
	begin

		case Handle.m_Colour_Format is

			when E_Grey  => Rotate_180_Grey (Handle.m_Data_Grey.Pixels.all);
			when E_GreyA => Rotate_180_GreyA (Handle.m_Data_GreyA.Pixels.all);
			when E_RGB   => Rotate_180_RGB (Handle.m_Data_RGB.Pixels.all);
			when E_RGBA  => Rotate_180_RGBA (Handle.m_Data_RGBA.Pixels.all);

			when E_Undefined => raise EX_INVALID_HANDLE;

		end case;

	end Rotate_180;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Insert_Into (
		Handle : in     T_Image;
		Target : in out T_Image'Class;
		X, Y   : in     T_Size;
		Region : in     T_Region := (others => <>)
	) is

		Colour_Format    : constant T_Colour_Format := Handle.m_Colour_Format;
		Corrected_Region : T_Region;

	begin

		if Colour_Format = E_Undefined or else Target.m_Colour_Format = E_Undefined then
			raise EX_INVALID_HANDLE;

		elsif Colour_Format /= Target.m_Colour_Format then
			raise EX_WRONG_COLOUR_FORMAT;
		end if;

		Corrected_Region := Sanitise_Region (Region, Handle.Get_Width, Handle.Get_Height);

		case Colour_Format is

				when E_Grey  => Insert_Grey  (Handle.m_Data_Grey.Pixels.all,  Target.m_Data_Grey.Pixels.all,  X, Y, Corrected_Region);
				when E_GreyA => Insert_GreyA (Handle.m_Data_GreyA.Pixels.all, Target.m_Data_GreyA.Pixels.all, X, Y, Corrected_Region);
				when E_RGB   => Insert_RGB   (Handle.m_Data_RGB.Pixels.all,   Target.m_Data_RGB.Pixels.all,   X, Y, Corrected_Region);
				when E_RGBA  => Insert_RGBA  (Handle.m_Data_RGBA.Pixels.all,  Target.m_Data_RGBA.Pixels.all,  X, Y, Corrected_Region);

				when E_Undefined => null;

		end case;

	end Insert_Into;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Crop (
		Handle : in out T_Image;
		Region : in     T_Region
	) is
	begin

		Handle.Crop (Handle, Region);

	end Crop;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Crop (
		Handle : in     T_Image;
		Target :    out T_Image'Class;
		Region : in     T_Region
	) is

		Colour_Format    : constant T_Colour_Format := Handle.m_Colour_Format;
		Corrected_Region : T_Region;
		Width, Height    : T_Size'Base;

	begin

		if Colour_Format = E_Undefined then
			raise EX_INVALID_HANDLE;
		end if;

		Width            := Handle.Get_Width;
		Height           := Handle.Get_Height;
		Corrected_Region := Sanitise_Region (Region, Width, Height);

		Width  := 1 + Corrected_Region.Right  - Corrected_Region.Left;
		Height := 1 + Corrected_Region.Bottom - Corrected_Region.Top;

		Target.Create (Width, Height, Colour_Format);

		case Colour_Format is

				when E_Grey  => Insert_Grey  (Handle.m_Data_Grey.Pixels.all,  Target.m_Data_Grey.Pixels.all,  1, 1, Corrected_Region);
				when E_GreyA => Insert_GreyA (Handle.m_Data_GreyA.Pixels.all, Target.m_Data_GreyA.Pixels.all, 1, 1, Corrected_Region);
				when E_RGB   => Insert_RGB   (Handle.m_Data_RGB.Pixels.all,   Target.m_Data_RGB.Pixels.all,   1, 1, Corrected_Region);
				when E_RGBA  => Insert_RGBA  (Handle.m_Data_RGBA.Pixels.all,  Target.m_Data_RGBA.Pixels.all,  1, 1, Corrected_Region);

				when E_Undefined => null;

		end case;

	end Crop;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Get_Const_Data_Grey (Handle : in T_Image) return access constant T_Colour_Grey_Arr is
	begin

		if Handle.m_Colour_Format = E_Undefined then
			raise EX_INVALID_HANDLE;

		elsif Handle.m_Colour_Format /= E_Grey then
			raise EX_WRONG_COLOUR_FORMAT;
		end if;

		return Handle.m_Data_Grey.Pixels;

	end Get_Const_Data_Grey;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Get_Const_Data_GreyA (Handle : in T_Image) return access constant T_Colour_GreyA_Arr is
	begin

		if Handle.m_Colour_Format = E_Undefined then
			raise EX_INVALID_HANDLE;

		elsif Handle.m_Colour_Format /= E_GreyA then
			raise EX_WRONG_COLOUR_FORMAT;
		end if;

		return Handle.m_Data_GreyA.Pixels;

	end Get_Const_Data_GreyA;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Get_Const_Data_RGB (Handle : in T_Image) return access constant T_Colour_RGB_Arr is
	begin

		if Handle.m_Colour_Format = E_Undefined then
			raise EX_INVALID_HANDLE;

		elsif Handle.m_Colour_Format /= E_RGB then
			raise EX_WRONG_COLOUR_FORMAT;
		end if;

		return Handle.m_Data_RGB.Pixels;

	end Get_Const_Data_RGB;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Get_Const_Data_RGBA (Handle : in T_Image) return access constant T_Colour_RGBA_Arr is
	begin

		if Handle.m_Colour_Format = E_Undefined then
			raise EX_INVALID_HANDLE;

		elsif Handle.m_Colour_Format /= E_RGBA then
			raise EX_WRONG_COLOUR_FORMAT;
		end if;

		return Handle.m_Data_RGBA.Pixels;

	end Get_Const_Data_RGBA;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Get_Data_Grey (Handle : in out T_Image) return access T_Colour_Grey_Arr is
	begin

		if Handle.m_Colour_Format = E_Undefined then
			raise EX_INVALID_HANDLE;

		elsif Handle.m_Colour_Format /= E_Grey then
			raise EX_WRONG_COLOUR_FORMAT;
		end if;

		return Handle.m_Data_Grey.Pixels;

	end Get_Data_Grey;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Get_Data_GreyA (Handle : in out T_Image) return access T_Colour_GreyA_Arr is
	begin

		if Handle.m_Colour_Format = E_Undefined then
			raise EX_INVALID_HANDLE;

		elsif Handle.m_Colour_Format /= E_GreyA then
			raise EX_WRONG_COLOUR_FORMAT;
		end if;

		return Handle.m_Data_GreyA.Pixels;

	end Get_Data_GreyA;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Get_Data_RGB (Handle : in out T_Image) return access T_Colour_RGB_Arr is
	begin

		if Handle.m_Colour_Format = E_Undefined then
			raise EX_INVALID_HANDLE;

		elsif Handle.m_Colour_Format /= E_RGB then
			raise EX_WRONG_COLOUR_FORMAT;
		end if;

		return Handle.m_Data_RGB.Pixels;

	end Get_Data_RGB;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Get_Data_RGBA (Handle : in out T_Image) return access T_Colour_RGBA_Arr is
	begin

		if Handle.m_Colour_Format = E_Undefined then
			raise EX_INVALID_HANDLE;

		elsif Handle.m_Colour_Format /= E_RGBA then
			raise EX_WRONG_COLOUR_FORMAT;
		end if;

		return Handle.m_Data_RGBA.Pixels;

	end Get_Data_RGBA;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Get_Colour_Format (Handle : in T_Image) return T_Colour_Format is
	begin

		return Handle.m_Colour_Format;

	end Get_Colour_Format;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Get_Width (Handle : in T_Image) return T_Size is
	begin

		case Handle.m_Colour_Format is

			when E_Grey  => return Handle.m_Data_Grey.Width;
			when E_GreyA => return Handle.m_Data_GreyA.Width;
			when E_RGB   => return Handle.m_Data_RGB.Width;
			when E_RGBA  => return Handle.m_Data_RGBA.Width;

			when E_Undefined => raise EX_INVALID_HANDLE;

		end case;

	end Get_Width;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Get_Height (Handle : in T_Image) return T_Size is
	begin

		case Handle.m_Colour_Format is

			when E_Grey  => return Handle.m_Data_Grey.Height;
			when E_GreyA => return Handle.m_Data_GreyA.Height;
			when E_RGB   => return Handle.m_Data_RGB.Height;
			when E_RGBA  => return Handle.m_Data_RGBA.Height;

			when E_Undefined => raise EX_INVALID_HANDLE;

		end case;

	end Get_Height;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Save (
		Handle           : in T_Image;
		File_Path        : in String;
		File_Format      : in T_File_Format;
		Append_Extension : in Boolean             := true;
		Compression      : in T_Compression_Level := 90
	) is
	begin

		case File_Format is
			when E_BMP  => Handle.Save_As_BMP (File_Path, Append_Extension);
			when E_PNG  => Handle.Save_As_PNG (File_Path, Append_Extension);
			when E_TGA  => Handle.Save_As_TGA (File_Path, Append_Extension);
			when E_JPEG => Handle.Save_As_JPEG (File_Path, Append_Extension, Compression);
		end case;

	end Save;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Save_As_BMP (
		Handle           : in T_Image;
		File_Path        : in String;
		Append_Extension : in Boolean := true
	) is

		Path_Sanitised : constant String := (
			Sanitise_File_Path (File_Path) & (if Append_Extension then "." & To_String (E_BMP) else "")
		);

		Width    : constant T_Natural     := T_Natural (Handle.Get_Width);
		Height   : constant T_Natural     := T_Natural (Handle.Get_Height);
		Channels : constant T_Natural     := T_Colour_Format'Pos (Handle.m_Colour_Format);
		Address  : constant Cre8or_stb.T_Address := Handle.Get_Data_Address;

	begin

		Create_File_Path (Path_Sanitised);
		Cre8or_stb.Images.Write_BMP (Path_Sanitised, Width, Height, Channels, Address);

	end Save_As_BMP;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Save_As_PNG (
		Handle           : in T_Image;
		File_Path        : in String;
		Append_Extension : in Boolean := true
	) is

		Path_Sanitised : constant String := (
			Sanitise_File_Path (File_Path) & (if Append_Extension then "." & To_String (E_PNG) else "")
		);

		Width    : constant T_Natural     := T_Natural (Handle.Get_Width);
		Height   : constant T_Natural     := T_Natural (Handle.Get_Height);
		Channels : constant T_Natural     := T_Colour_Format'Pos (Handle.m_Colour_Format);
		Address  : constant Cre8or_stb.T_Address := Handle.Get_Data_Address;

	begin

		Create_File_Path (Path_Sanitised);
		Cre8or_stb.Images.Write_PNG (Path_Sanitised, Width, Height, Channels, Address);

	end Save_As_PNG;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Save_As_TGA (
		Handle           : in T_Image;
		File_Path        : in String;
		Append_Extension : in Boolean := true
	) is

		Path_Sanitised : constant String := (
			Sanitise_File_Path (File_Path) & (if Append_Extension then "." & To_String (E_TGA) else "")
		);

		Width    : constant T_Natural     := T_Natural (Handle.Get_Width);
		Height   : constant T_Natural     := T_Natural (Handle.Get_Height);
		Channels : constant T_Natural     := T_Colour_Format'Pos (Handle.m_Colour_Format);
		Address  : constant Cre8or_stb.T_Address := Handle.Get_Data_Address;

	begin

		Create_File_Path (Path_Sanitised);
		Cre8or_stb.Images.Write_PNG (Path_Sanitised, Width, Height, Channels, Address);

	end Save_As_TGA;

	---------------------------------------------------------------------------------------------------------------------
	not overriding procedure Save_As_JPEG (
		Handle           : in T_Image;
		File_Path        : in String;
		Append_Extension : in Boolean             := true;
		Compression      : in T_Compression_Level := 90
	) is

		Path_Sanitised : constant String := (
			Sanitise_File_Path (File_Path) & (if Append_Extension then "." & To_String (E_JPEG) else "")
		);

		Width    : constant T_Natural     := T_Natural (Handle.Get_Width);
		Height   : constant T_Natural     := T_Natural (Handle.Get_Height);
		Channels : constant T_Natural     := T_Colour_Format'Pos (Handle.m_Colour_Format);
		Address  : constant Cre8or_stb.T_Address := Handle.Get_Data_Address;

	begin

		Create_File_Path (Path_Sanitised);
		Cre8or_stb.Images.Write_JPG (Path_Sanitised, Width, Height, Channels, Address, Cre8or_stb.T_Integer (Compression));

	end Save_As_JPEG;



	-- Bodies
	---------------------------------------------------------------------------------------------------------------------
	function "="(Left, Right : in T_Image) return Boolean is
	begin

		if Left.m_Colour_Format /= Right.m_Colour_Format then
			return false;
		end if;

		case Left.m_Colour_Format is

			when E_Grey  => return Left.m_Data_Grey  = Right.m_Data_Grey;
			when E_GreyA => return Left.m_Data_GreyA = Right.m_Data_GreyA;
			when E_RGB   => return Left.m_Data_RGB   = Right.m_Data_RGB;
			when E_RGBA  => return Left.m_Data_RGBA  = Right.m_Data_RGBA;

			when E_Undefined => return false;

		end case;

	end "=";

	---------------------------------------------------------------------------------------------------------------------
	function To_Float (Value : in T_Byte) return T_Float is
	begin

		return T_Float'Base (Value) / 255.0;

	end To_Float;

	---------------------------------------------------------------------------------------------------------------------
	function To_Byte (Value : in T_Float) return T_Byte is
	begin

		if Value >= 1.0 then
			return 255;
		elsif Value <= 0.0 then
			return 0;
		end if;

		return T_Byte (Value * 255.0);

	end To_Byte;

	---------------------------------------------------------------------------------------------------------------------
	function To_Float (Colour : in T_Colour_GreyA) return T_Colour_GreyA_Float is
	begin

		return (
			To_Float (Colour (C_GreyA_G)),
			To_Float (Colour (C_GreyA_A))
		);

	end To_Float;

	---------------------------------------------------------------------------------------------------------------------
	function To_Byte (Colour : in T_Colour_GreyA_Float) return T_Colour_GreyA is
	begin

		return (
			To_Byte (Colour (C_GreyA_G)),
			To_Byte (Colour (C_GreyA_A))
		);

	end To_Byte;

	---------------------------------------------------------------------------------------------------------------------
	function To_Float (Colour : in T_Colour_RGB) return T_Colour_RGB_Float is
	begin

		return (
			To_Float (Colour (C_RGBA_R)),
			To_Float (Colour (C_RGBA_G)),
			To_Float (Colour (C_RGBA_B))
		);

	end To_Float;

	---------------------------------------------------------------------------------------------------------------------
	function To_Byte (Colour : in T_Colour_RGB_Float) return T_Colour_RGB is
	begin

		return (
			To_Byte (Colour (C_RGBA_R)),
			To_Byte (Colour (C_RGBA_G)),
			To_Byte (Colour (C_RGBA_B))
		);

	end To_Byte;

	---------------------------------------------------------------------------------------------------------------------
	function To_Float (Colour : in T_Colour_RGBA) return T_Colour_RGBA_Float is
	begin

		return (
			To_Float (Colour (C_RGBA_R)),
			To_Float (Colour (C_RGBA_G)),
			To_Float (Colour (C_RGBA_B)),
			To_Float (Colour (C_RGBA_A))
		);

	end To_Float;

	---------------------------------------------------------------------------------------------------------------------
	function To_Byte (Colour : in T_Colour_RGBA_Float) return T_Colour_RGBA is
	begin

		return (
			To_Byte (Colour (C_RGBA_R)),
			To_Byte (Colour (C_RGBA_G)),
			To_Byte (Colour (C_RGBA_B)),
			To_Byte (Colour (C_RGBA_A))
		);

	end To_Byte;

	---------------------------------------------------------------------------------------------------------------------
	function To_String (File_Format : in T_File_Format) return String is
	begin

		case File_Format is

			when E_BMP  => return "bmp";
			when E_PNG  => return "png";
			when E_TGA  => return "tga";
			when E_JPEG => return "jpg";

		end case;

	end To_String;




-- PRIVATE



	-- Primitives
	---------------------------------------------------------------------------------------------------------------------
	-- T_Handle
	---------------------------------------------------------------------------------------------------------------------
	overriding procedure Adjust (Handle : in out T_Image) is
	begin

		case Handle.m_Colour_Format is

			when E_Grey  => System.Atomic_Counters.Increment (Handle.m_Data_Grey.Ref_Count);
			when E_GreyA => System.Atomic_Counters.Increment (Handle.m_Data_GreyA.Ref_Count);
			when E_RGB   => System.Atomic_Counters.Increment (Handle.m_Data_RGB.Ref_Count);
			when E_RGBA  => System.Atomic_Counters.Increment (Handle.m_Data_RGBA.Ref_Count);

			when E_Undefined => null;

		end case;

	end Adjust;

	---------------------------------------------------------------------------------------------------------------------
	overriding procedure Finalize (Handle : in out T_Image) is
	begin

		case Handle.m_Colour_Format is

			when E_Grey  =>
				Drop_Reference (Handle.m_Data_Grey);
				Handle.m_Data_Grey  := null;

			when E_GreyA =>
				Drop_Reference (Handle.m_Data_GreyA);
				Handle.m_Data_GreyA := null;

			when E_RGB   =>
				Drop_Reference (Handle.m_Data_RGB);
				Handle.m_Data_RGB   := null;

			when E_RGBA  =>
				Drop_Reference (Handle.m_Data_RGBA);
				Handle.m_Data_RGBA  := null;

			when E_Undefined => null;

		end case;

		Handle.m_Colour_Format := E_Undefined;

	end Finalize;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Get_Data_Address (Handle : in T_Image) return Cre8or_stb.T_Address is
	begin

		case Handle.m_Colour_Format is

			when E_Grey  => return Handle.m_Data_Grey.Pixels (1,1)'Address;
			when E_GreyA => return Handle.m_Data_GreyA.Pixels (1,1)'Address;
			when E_RGB   => return Handle.m_Data_RGB.Pixels (1,1)'Address;
			when E_RGBA  => return Handle.m_Data_RGBA.Pixels (1,1)'Address;

			when E_Undefined => raise EX_WRONG_COLOUR_FORMAT;

		end case;

	end Get_Data_Address;

	---------------------------------------------------------------------------------------------------------------------
	not overriding function Get_Data_Length (Handle : in T_Image) return T_Natural is
	begin

		case Handle.m_Colour_Format is

			when E_Grey  => return Handle.m_Data_Grey.Pixels.all'Size  / T_Byte'Size;
			when E_GreyA => return Handle.m_Data_GreyA.Pixels.all'Size / T_Byte'Size;
			when E_RGB   => return Handle.m_Data_RGB.Pixels.all'Size   / T_Byte'Size;
			when E_RGBA  => return Handle.m_Data_RGBA.Pixels.all'Size  / T_Byte'Size;

			when E_Undefined => raise EX_WRONG_COLOUR_FORMAT;

		end case;

	end Get_Data_Length;



	-- Bodies
	---------------------------------------------------------------------------------------------------------------------
	procedure Drop_Reference (Data : in out T_Data_Grey_Ref) is
	begin

		if System.Atomic_Counters.Decrement (Data.Ref_Count) then
			Deallocate (Data.Pixels);
			Deallocate (Data);
			return;
		end if;

	end Drop_Reference;

	---------------------------------------------------------------------------------------------------------------------
	procedure Drop_Reference (Data : in out T_Data_GreyA_Ref) is
	begin

		if System.Atomic_Counters.Decrement (Data.Ref_Count) then
			Deallocate (Data.Pixels);
			Deallocate (Data);
			return;
		end if;

	end Drop_Reference;

	---------------------------------------------------------------------------------------------------------------------
	procedure Drop_Reference (Data : in out T_Data_RGB_Ref) is
	begin

		if System.Atomic_Counters.Decrement (Data.Ref_Count) then
			Deallocate (Data.Pixels);
			Deallocate (Data);
			return;
		end if;

	end Drop_Reference;

	---------------------------------------------------------------------------------------------------------------------
	procedure Drop_Reference (Data : in out T_Data_RGBA_Ref) is
	begin

		if System.Atomic_Counters.Decrement (Data.Ref_Count) then
			Deallocate (Data.Pixels);
			Deallocate (Data);
			return;
		end if;

	end Drop_Reference;

	---------------------------------------------------------------------------------------------------------------------
	procedure Create_File_Path (Sanitised_File_Path : in String) is

		First     : Natural := Sanitised_File_Path'First;
		Pos_Slash : Natural := 0;

	begin

		-- Find the last slash
		Loop_Slash :
		for I in reverse Sanitised_File_Path'Range loop

			if Sanitised_File_Path (I) in '\' | '/' then
				if
					I - 1 >= First
					and then Sanitised_File_Path (I - 1) not in '\' | '/'
				then
					Pos_Slash := I;
					exit Loop_Slash;
				end if;
			end if;

		end loop Loop_Slash;

		-- Ensure the folder path exists (stb expects it)
		if Pos_Slash > 0 then
			Ada.Directories.Create_Path (
				Sanitised_File_Path (First .. Pos_Slash - 1)
			);
		end if;

	end Create_File_Path;

	---------------------------------------------------------------------------------------------------------------------
	function Sanitise_File_Path (File_Path : in String) return String is

		use Ada.Strings;

		Trimmed     : String   := Fixed.Trim (File_Path, Both);
		First       : Positive := Trimmed'First;
		Char        : Character;

	begin

		-- Remove the leading slashes (stb doesn't seem to tolerate them).
		Loop_Slash :
		for I in Trimmed'Range loop

			Char := Trimmed (I);

			-- Replace backwards slashes with forward slashes (unix compatibility)
			if Char = '\' then
				Trimmed (I) := '/';
			end if;

		end loop Loop_Slash;

		return Trimmed (First .. Trimmed'Last);

	end Sanitise_File_Path;

	---------------------------------------------------------------------------------------------------------------------
	function Sanitise_Region (
		Region : in T_Region;
		Width  : in T_Size;
		Height : in T_Size
	) return T_Region is

		Top    : T_Size'Base := T_Size'Max (1,      Region.Top);    -- 1    .. inf
		Left   : T_Size'Base := T_Size'Max (1,      Region.Left);   -- 1    .. inf
		Bottom : T_Size'Base := T_Size'Min (Height, Region.Bottom); -- -inf .. Height
		Right  : T_Size'Base := T_Size'Min (Width,  Region.Right);  -- -inf .. Width

	begin

		Top    := T_Size'Min (Top,  Height); -- 1    .. Height
		Left   := T_Size'Min (Left, Width);  -- 1    .. Width
		Bottom := T_Size'Max (Top,  Bottom); -- Top  .. Height
		Right  := T_Size'Max (Left, Right);  -- Left .. Width

		return (
			Top    => Top,
			Left   => Left,
			Bottom => Bottom,
			Right  => Right
		);

	end Sanitise_Region;



end Cre8or_Generic_Image_Handling;

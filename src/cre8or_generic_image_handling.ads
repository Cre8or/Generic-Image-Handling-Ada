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





private with Ada.Finalization;
private with Ada.Unchecked_Deallocation;
private with System.Atomic_Counters;

private with Cre8or_stb.Images;



pragma Elaborate_All (Ada.Finalization);
pragma Elaborate_All (Ada.Unchecked_Deallocation);
pragma Elaborate_All (System.Atomic_Counters);

pragma Elaborate_All (Cre8or_stb.Images);





generic

	-- Must be an 8 bit modular type
	type T_Byte is mod <>;

	-- Must include the range 0.0 .. 1.0
	type T_Float is digits <>;

	-- Used to access individual pixels of an image
	-- May include negative values, however, image coordinates will always start at 1,1
	type T_Size is range <>;

	-- Used to access components of greyscale-alpha colours
	type T_Index_GreyA is (<>);

	-- Used to access components of RGB (and RGBA) colours
	type T_Index_RGBA is (<>);



package Cre8or_Generic_Image_Handling is



	-- Validate the actuals
	pragma Compile_Time_Error (
		T_Byte'Size /= 8,
		"Actual for T_Byte must be an 8 bit modular type"
	);

	pragma Compile_Time_Error (
		0.0 not in T_Float'Range or 1.0 not in T_Float'Range,
		"Actual for T_Float must include the range 0.0 .. 1.0"
	);

	pragma Compile_Time_Error (
		1 not in T_Size'Range,
		"Actual for T_Size must include the value 1"
	);

	pragma Compile_Time_Error (
		T_Index_GreyA'Pos (T_Index_GreyA'Last) + 1 - T_Index_GreyA'Pos (T_Index_GreyA'First) /= 2,
		"Actual for T_Index_GreyA must have exactly 2 values"
	);

	pragma Compile_Time_Error (
		T_Index_RGBA'Pos (T_Index_RGBA'Last) + 1 - T_Index_RGBA'Pos (T_Index_RGBA'First) /= 4,
		"Actual for T_Index_RGBA must have exactly 4 values"
	);

	pragma Compile_Time_Warning (
		T_Size'Last < 4096,
		"Range of actual for T_Size may be too short" & ASCII.LF &
		"Instantiation won't support images larger than 4k resolution"
	);



	-- Exceptions
	EX_INDEX_OUT_OF_BOUNDS : exception;
	EX_INVALID_HANDLE      : exception;
	EX_INVALID_FILE_FORMAT : exception;
	EX_FILE_READ_ERROR     : exception;
	EX_WRONG_COLOUR_FORMAT : exception;



	-- Types
	type T_Colour_Format is (
		E_Undefined,
		E_Grey,
		E_GreyA,
		E_RGB,
		E_RGBA
	);

	type T_Colour_GreyA is array (T_Index_GreyA) of aliased T_Byte;
	for T_Colour_GreyA'Component_Size use 8;

	type T_Colour_RGB is array (T_Index_RGBA range T_Index_RGBA'First .. T_Index_RGBA'Pred (T_Index_RGBA'Last)) of aliased T_Byte;
	for T_Colour_RGB'Component_Size use 8;

	type T_Colour_RGBA is array (T_Index_RGBA) of aliased T_Byte;
	for T_Colour_RGBA'Component_Size use 8;

	type T_Colour_GreyA_Float is array (T_Index_GreyA) of T_Float;

	type T_Colour_RGB_Float is array (T_Index_RGBA range T_Index_RGBA'First .. T_Index_RGBA'Pred (T_Index_RGBA'Last)) of T_Float;

	type T_Colour_RGBA_Float is array (T_Index_RGBA) of T_Float;

	-- NOTE: Accessing colours is done as: Image_Data (Column, Row), or Image (Y, X)!
	-- This is called row-major order, and is the standard used in C (and apparently, Ada).
	-- See: https://en.wikipedia.org/wiki/Row-_and_column-major_order
	type T_Colour_Grey_Arr is array (T_Size range <>, T_Size range <>) of aliased T_Byte;

	type T_Colour_GreyA_Arr is array (T_Size range <>, T_Size range <>) of aliased T_Colour_GreyA;

	type T_Colour_RGB_Arr is array (T_Size range <>, T_Size range <>) of aliased T_Colour_RGB;

	type T_Colour_RGBA_Arr is array (T_Size range <>, T_Size range <>) of aliased T_Colour_RGBA;

	type T_File_Format is (
		E_BMP,
		E_PNG,
		E_TGA,
		E_JPEG
	);

	type T_Region is record
		Top    : T_Size := T_Size'First;
		Left   : T_Size := T_Size'First;
		Bottom : T_Size := T_Size'Last;
		Right  : T_Size := T_Size'Last;
	end record;

	type T_Compression_Level is range 1 .. 100;

	type T_Image is tagged private;

		-- Primitives
		-----------------------------------------------------------------------------------------------------------------
		-- Creates a blank image with the given size, colour format and default value (applied to every channel).
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Create (
			Handle        : in out T_Image;
			Width, Height : in     T_Size;
			Colour_Format : in     T_Colour_Format;
			Default_Value : in     T_Byte := 255
		);

		-----------------------------------------------------------------------------------------------------------------
		-- Creates an image from the given greyscale colour array.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Create_From (
			Handle : in out T_Image;
			Data   : in     T_Colour_Grey_Arr
		);

		-----------------------------------------------------------------------------------------------------------------
		-- Creates an image from the given greyscale-alpha colour array.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Create_From (
			Handle : in out T_Image;
			Data   : in     T_Colour_GreyA_Arr
		);

		-----------------------------------------------------------------------------------------------------------------
		-- Creates an image from the given RGB colour array.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Create_From (
			Handle : in out T_Image;
			Data   : in     T_Colour_RGB_Arr
		);

		-----------------------------------------------------------------------------------------------------------------
		-- Creates an image from the given RGBA colour array.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Create_From (
			Handle : in out T_Image;
			Data   : in     T_Colour_RGBA_Arr
		);

		-----------------------------------------------------------------------------------------------------------------
		-- Clears an image. This resets the handle, but not necessarily the image data.
		-- NOTE: The actual image data will only be deleted once all references to it (via other handles) are dropped.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Clear (Handle : in out T_Image)
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Converts an image from one colour format into another. This creates a new image in the process.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-- NOTE 1: When converting to a lower number of channels (e.g. RGBA to greyscale), only the first N channels that
		-- are needed are copied over (in this example, red becomes the greyscale channel). All remaining channels are
		-- discarded.
		-- When converting to a higher number of channels (e.g. greyscale to RGBA), the first N channels that are needed
		-- are copied over (greyscale becomes red), and the provided default value is used to fill in the other channels.
		-- NOTE 2: Setting the format to E_Undefined causes the image to be cleared.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Convert_To (
			Handle        : in out T_Image;
			Colour_Format : in     T_Colour_Format;
			Default_Value : in     T_Byte := 255
		) with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Converts an image from one colour format into another. This creates a new image in the process.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-- NOTE 1: When converting to a lower number of channels (e.g. RGBA to greyscale), only the first N channels that
		-- are needed are copied over (in this example, red becomes the greyscale channel). All remaining channels are
		-- discarded.
		-- When converting to a higher number of channels (e.g. greyscale to RGBA), the first N channels that are needed
		-- are copied over (greyscale becomes red), and the provided default value is used to fill in the other channels.
		-- NOTE 2: Setting the format to E_Undefined causes the image to be cleared.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Convert_To (
			Handle        : in     T_Image;
			Target        :    out T_Image'Class;
			Colour_Format : in     T_Colour_Format;
			Default_Value : in     T_Byte := 255
		);

		-----------------------------------------------------------------------------------------------------------------
		-- Returns true if the given image is initialised (has data), otherwise false.
		-----------------------------------------------------------------------------------------------------------------
		not overriding function Is_Initialised (Handle : in T_Image) return Boolean
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Loads an image from the given file.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Load (
			Handle    : in out T_Image;
			File_Path : in     String
		);

		-----------------------------------------------------------------------------------------------------------------
		-- Performs a deep copy of the image to the given target.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Copy_To (
			Handle : in     T_Image;
			Target :    out T_Image'Class
		);

		-----------------------------------------------------------------------------------------------------------------
		-- Flips the image horizontally.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Flip_Horizontally (Handle : in out T_Image)
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Flips the image vertically.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Flip_Vertically (Handle : in out T_Image)
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Rotates the image 90° clockwise.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Rotate_CW (Handle : in out T_Image)
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Rotates the image 90° counter-clockwise.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Rotate_CCW (Handle : in out T_Image)
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Rotates the image 180°.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Rotate_180 (Handle : in out T_Image)
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Inserts the source image (or a region of it) into the target image at the specified X and Y coordinates, overwriting
		-- any underlying data in the process (no blending is performed).
		-- Any source data that lands outside of the target image's dimensions will be discarded.
		-- If either the source of target image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-- If both images are not in the same colour format, the exception EX_WRONG_COLOUR_FORMAT is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Insert_Into (
			Handle : in     T_Image;
			Target : in out T_Image'Class;
			X, Y   : in     T_Size;
			Region : in     T_Region := (others => <>)
		);

		-----------------------------------------------------------------------------------------------------------------
		-- Crops the image to the specified region.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Crop (
			Handle : in out T_Image;
			Region : in     T_Region
		) with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Crops the image to the specified region and stores the resulting image in the target handle.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Crop (
			Handle : in     T_Image;
			Target :    out T_Image'Class;
			Region : in     T_Region
		);

		-----------------------------------------------------------------------------------------------------------------
		-- Returns a read-only access to the colour array of an image in greyscale format. Intended for interfacing with
		-- other libraries (such as rendering APIs). Use with care.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-- If the image is not in colour format E_Grey, the exception EX_WRONG_COLOUR_FORMAT is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding function Get_Const_Data_Grey (Handle : in T_Image) return access constant T_Colour_Grey_Arr
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Returns a read-only access to the colour array of an image in greyscale-alpha format. Intended for interfacing
		-- with other libraries (such as rendering APIs). Use with care.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-- If the image is not in colour format E_GreyA, the exception EX_WRONG_COLOUR_FORMAT is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding function Get_Const_Data_GreyA (Handle : in T_Image) return access constant T_Colour_GreyA_Arr
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Returns a read-only access to the colour array of an image in RGB format. Intended for interfacing with other
		-- libraries (such as rendering APIs). Use with care.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-- If the image is not in colour format E_RGB, the exception EX_WRONG_COLOUR_FORMAT is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding function Get_Const_Data_RGB (Handle : in T_Image) return access constant T_Colour_RGB_Arr
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Returns a read-only access to the colour array of an image in RGBA format. Intended for interfacing with other
		-- libraries (such as rendering APIs). Use with care.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-- If the image is not in colour format E_RGBA, the exception EX_WRONG_COLOUR_FORMAT is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding function Get_Const_Data_RGBA (Handle : in T_Image) return access constant T_Colour_RGBA_Arr
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Returns a writable access to the colour array of an image in greyscale format. Intended for interfacing with
		-- other libraries (such as rendering APIs). Use with care.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-- If the image is not in colour format E_Grey, the exception EX_WRONG_COLOUR_FORMAT is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding function Get_Data_Grey (Handle : in out T_Image) return access T_Colour_Grey_Arr
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Returns a writable access to the colour array of an image in greyscale-alpha format. Intended for interfacing
		-- with other libraries (such as rendering APIs). Use with care.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-- If the image is not in colour format E_GreyA, the exception EX_WRONG_COLOUR_FORMAT is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding function Get_Data_GreyA (Handle : in out T_Image) return access T_Colour_GreyA_Arr
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Returns a writable access to the colour array of an image in RGB format. Intended for interfacing with other
		-- libraries (such as rendering APIs). Use with care.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-- If the image is not in colour format E_RGB, the exception EX_WRONG_COLOUR_FORMAT is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding function Get_Data_RGB (Handle : in out T_Image) return access T_Colour_RGB_Arr
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Returns a writable access to the colour array of an image in RGBA format. Intended for interfacing with other
		-- libraries (such as rendering APIs). Use with care.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-- If the image is not in colour format E_RGBA, the exception EX_WRONG_COLOUR_FORMAT is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding function Get_Data_RGBA (Handle : in out T_Image) return access T_Colour_RGBA_Arr
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Returns the image's colour format.
		-----------------------------------------------------------------------------------------------------------------
		not overriding function Get_Colour_Format (Handle : in T_Image) return T_Colour_Format
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Returns the image's width.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding function Get_Width (Handle : in T_Image) return T_Size
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Returns the image's height.
		-- If the image is not initialised, the exception EX_INVALID_HANDLE is raised.
		-----------------------------------------------------------------------------------------------------------------
		not overriding function Get_Height (Handle : in T_Image) return T_Size
		with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Saves the image to the given file path using the specified file format.
		-- Compression is only relevant when saving images in JPEG format.
		-- If Append_Extension is true, the file format's respective extension is added at the end of the file name.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Save (
			Handle           : in T_Image;
			File_Path        : in String;
			File_Format      : in T_File_Format;
			Append_Extension : in Boolean             := true;
			Compression      : in T_Compression_Level := 90
		) with Inline;

		-----------------------------------------------------------------------------------------------------------------
		-- Saves the image as BMP to the given file path.
		-- If Append_Extension is true, the file format's respective extension is added at the end of the file name.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Save_As_BMP (
			Handle           : in T_Image;
			File_Path        : in String;
			Append_Extension : in Boolean := true
		);

		-----------------------------------------------------------------------------------------------------------------
		-- Saves the image as PNG to the given file path.
		-- If Append_Extension is true, the file format's respective extension is added at the end of the file name.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Save_As_PNG (
			Handle           : in T_Image;
			File_Path        : in String;
			Append_Extension : in Boolean := true
		);

		-----------------------------------------------------------------------------------------------------------------
		-- Saves the image as TGA to the given file path.
		-- If Append_Extension is true, the file format's respective extension is added at the end of the file name.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Save_As_TGA (
			Handle           : in T_Image;
			File_Path        : in String;
			Append_Extension : in Boolean := true
		);

		-----------------------------------------------------------------------------------------------------------------
		-- Saves the image as JPEG to the given file path with the specified compression level.
		-- If Append_Extension is true, the file format's respective extension is added at the end of the file name.
		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Save_As_JPEG (
			Handle           : in T_Image;
			File_Path        : in String;
			Append_Extension : in Boolean             := true;
			Compression      : in T_Compression_Level := 90
		);



	-- Specifications
	---------------------------------------------------------------------------------------------------------------------
	-- Returns true if two handles are identical, otherwise false.
	-- Shallow copies are considered to be identical, while deep copies are not (even if their pixel data is identical).
	-- NOTE: Comparing with an uninitialised handle always returns false.
	---------------------------------------------------------------------------------------------------------------------
	function "="(Left, Right : in T_Image) return Boolean;

	---------------------------------------------------------------------------------------------------------------------
	-- Converts a colour component from byte to float format (in range 0.0 .. 1.0).
	---------------------------------------------------------------------------------------------------------------------
	function To_Float (Value : in T_Byte) return T_Float;

	---------------------------------------------------------------------------------------------------------------------
	-- Converts a colour component from float to byte format.
	---------------------------------------------------------------------------------------------------------------------
	function To_Byte (Value : in T_Float) return T_Byte;

	---------------------------------------------------------------------------------------------------------------------
	-- Converts a greyscale-alpha colour from byte to float format.
	---------------------------------------------------------------------------------------------------------------------
	function To_Float (Colour : in T_Colour_GreyA) return T_Colour_GreyA_Float
	with Inline;

	---------------------------------------------------------------------------------------------------------------------
	--  Converts a greyscale-alpha colour from float to byte format.
	---------------------------------------------------------------------------------------------------------------------
	function To_Byte (Colour : in T_Colour_GreyA_Float) return T_Colour_GreyA
	with Inline;

	---------------------------------------------------------------------------------------------------------------------
	-- Converts an RGB colour from byte to float format.
	---------------------------------------------------------------------------------------------------------------------
	function To_Float (Colour : in T_Colour_RGB) return T_Colour_RGB_Float
	with Inline;

	---------------------------------------------------------------------------------------------------------------------
	-- Converts an RGB colour from float to byte format.
	---------------------------------------------------------------------------------------------------------------------
	function To_Byte (Colour : in T_Colour_RGB_Float) return T_Colour_RGB
	with Inline;

	---------------------------------------------------------------------------------------------------------------------
	-- Converts an RGBA colour from byte to float format.
	---------------------------------------------------------------------------------------------------------------------
	function To_Float (Colour : in T_Colour_RGBA) return T_Colour_RGBA_Float
	with Inline;

	---------------------------------------------------------------------------------------------------------------------
	-- Converts an RGBA colour from float to byte format.
	---------------------------------------------------------------------------------------------------------------------
	function To_Byte (Colour : in T_Colour_RGBA_Float) return T_Colour_RGBA
	with Inline;

	---------------------------------------------------------------------------------------------------------------------
	-- Returns the corresponding file extension string for the given file format, in lowercase.
	---------------------------------------------------------------------------------------------------------------------
	function To_String (File_Format : in T_File_Format) return String
	with Inline;



private



	-- use clauses
	use type Cre8or_stb.T_Integer;



	-- Constants
	C_GreyA_G : constant T_Index_GreyA := T_Index_GreyA'First;
	C_GreyA_A : constant T_Index_GreyA := T_Index_GreyA'Succ (C_GreyA_G);

	C_RGBA_R : constant T_Index_RGBA := T_Index_RGBA'First;
	C_RGBA_G : constant T_Index_RGBA := T_Index_RGBA'Succ (C_RGBA_R);
	C_RGBA_B : constant T_Index_RGBA := T_Index_RGBA'Succ (C_RGBA_G);
	C_RGBA_A : constant T_Index_RGBA := T_Index_RGBA'Succ (C_RGBA_B);



	-- Types
	subtype T_Natural is Cre8or_stb.T_Natural;

	type T_Byte_Arr is array (T_Natural range <>) of aliased T_Byte;

	type T_Colour_Grey_Arr_Ref  is access T_Colour_Grey_Arr;
	type T_Colour_GreyA_Arr_Ref is access T_Colour_GreyA_Arr;
	type T_Colour_RGB_Arr_Ref   is access T_Colour_RGB_Arr;
	type T_Colour_RGBA_Arr_Ref  is access T_Colour_RGBA_Arr;

	type T_Data_Grey is limited record
		Width     : T_Size;
		Height    : T_Size;
		Pixels    : T_Colour_Grey_Arr_Ref;
		Ref_Count : System.Atomic_Counters.Atomic_Counter;
	end record;

	type T_Data_Grey_Ref is access T_Data_Grey;

	type T_Data_GreyA is limited record
		Width     : T_Size;
		Height    : T_Size;
		Pixels    : T_Colour_GreyA_Arr_Ref;
		Ref_Count : System.Atomic_Counters.Atomic_Counter;
	end record;

	type T_Data_GreyA_Ref is access T_Data_GreyA;

	type T_Data_RGB is limited record
		Width     : T_Size;
		Height    : T_Size;
		Pixels    : T_Colour_RGB_Arr_Ref;
		Ref_Count : System.Atomic_Counters.Atomic_Counter;
	end record;

	type T_Data_RGB_Ref is access T_Data_RGB;

	type T_Data_RGBA is limited record
		Width     : T_Size;
		Height    : T_Size;
		Pixels    : T_Colour_RGBA_Arr_Ref;
		Ref_Count : System.Atomic_Counters.Atomic_Counter;
	end record;

	type T_Data_RGBA_Ref is access T_Data_RGBA;

	type T_Image is new Ada.Finalization.Controlled with record
		m_Colour_Format : T_Colour_Format := E_Undefined;

		m_Data_Grey  : T_Data_Grey_Ref;
		m_Data_GreyA : T_Data_GreyA_Ref;
		m_Data_RGB   : T_Data_RGB_Ref;
		m_Data_RGBA  : T_Data_RGBA_Ref;
	end record;

		-- Primitives
		-----------------------------------------------------------------------------------------------------------------
		overriding procedure Adjust (Handle : in out T_Image);

		-----------------------------------------------------------------------------------------------------------------
		overriding procedure Finalize (Handle : in out T_Image);

		-----------------------------------------------------------------------------------------------------------------
		not overriding procedure Clear (Handle : in out T_Image) renames Finalize;

		-----------------------------------------------------------------------------------------------------------------
		not overriding function Get_Data_Address (Handle : in T_Image) return Cre8or_stb.T_Address;

		-----------------------------------------------------------------------------------------------------------------
		not overriding function Get_Data_Length (Handle : in T_Image) return Cre8or_stb.T_Natural;



	-- Generics
	---------------------------------------------------------------------------------------------------------------------
	generic
		type T_Gen_Colour is private;
		type T_Gen_Colour_Arr is array (T_Size range <>, T_Size range <>) of T_Gen_Colour;
		type T_Gen_Colour_Arr_Ref is access T_Gen_Colour_Arr;
	function Gen_Parse_Raw_Data (
		Raw_Data : in Cre8or_stb.T_Data_Chunk;
		Width    : in T_Size;
		Height   : in T_Size
	) return T_Gen_Colour_Arr_Ref
	with Pre => Raw_Data.m_Length = T_Natural (Width * Height);

	---------------------------------------------------------------------------------------------------------------------
	generic
		type T_Gen_Colour is private;
		type T_Gen_Colour_Arr is array (T_Size range <>, T_Size range <>) of T_Gen_Colour;
		type T_Gen_Colour_Arr_Ref is access T_Gen_Colour_Arr;
	function Gen_Convert_Raw_Data (
		Raw_Data        : in Cre8or_stb.T_Data_Chunk;
		Width           : in T_Size;
		Height          : in T_Size;
		Source_Channels : in T_Natural;
		Target_Channels : in T_Natural;
		Fill_Value      : in T_Byte
	) return T_Gen_Colour_Arr_Ref;

	---------------------------------------------------------------------------------------------------------------------
	generic
		type T_Gen_Colour is private;
		type T_Gen_Colour_Arr is array (T_Size range <>, T_Size range <>) of T_Gen_Colour;
	procedure Gen_Flip_Horizontally (Data : in out T_Gen_Colour_Arr);

	---------------------------------------------------------------------------------------------------------------------
	generic
		type T_Gen_Colour is private;
		type T_Gen_Colour_Arr is array (T_Size range <>, T_Size range <>) of T_Gen_Colour;
	procedure Gen_Flip_Vertically (Data : in out T_Gen_Colour_Arr);

	---------------------------------------------------------------------------------------------------------------------
	generic
		type T_Gen_Colour is private;
		type T_Gen_Colour_Arr is array (T_Size range <>, T_Size range <>) of T_Gen_Colour;
		type T_Gen_Colour_Arr_Ref is access T_Gen_Colour_Arr;
		with procedure Deallocate (Data : in out T_Gen_Colour_Arr_Ref);
	procedure Gen_Rotate_CW (Data : in out T_Gen_Colour_Arr_Ref);

	---------------------------------------------------------------------------------------------------------------------
	generic
		type T_Gen_Colour is private;
		type T_Gen_Colour_Arr is array (T_Size range <>, T_Size range <>) of T_Gen_Colour;
		type T_Gen_Colour_Arr_Ref is access T_Gen_Colour_Arr;
		with procedure Deallocate (Data : in out T_Gen_Colour_Arr_Ref);
	procedure Gen_Rotate_CCW (Data : in out T_Gen_Colour_Arr_Ref);

	---------------------------------------------------------------------------------------------------------------------
	generic
		type T_Gen_Colour is private;
		type T_Gen_Colour_Arr is array (T_Size range <>, T_Size range <>) of T_Gen_Colour;
	procedure Gen_Rotate_180 (Data : in out T_Gen_Colour_Arr);

	---------------------------------------------------------------------------------------------------------------------
	generic
		type T_Gen_Colour is private;
		type T_Gen_Colour_Arr is array (T_Size range <>, T_Size range <>) of T_Gen_Colour;
	procedure Gen_Insert (
		Source   : in     T_Gen_Colour_Arr;
		Target   : in out T_Gen_Colour_Arr;
		X_Insert : in     T_Size;
		Y_Insert : in     T_Size;
		Region   : in     T_Region
	);



	-- Specifications
	---------------------------------------------------------------------------------------------------------------------
	procedure Drop_Reference (Data : in out T_Data_Grey_Ref);

	---------------------------------------------------------------------------------------------------------------------
	procedure Drop_Reference (Data : in out T_Data_GreyA_Ref);

	---------------------------------------------------------------------------------------------------------------------
	procedure Drop_Reference (Data : in out T_Data_RGB_Ref);

	---------------------------------------------------------------------------------------------------------------------
	procedure Drop_Reference (Data : in out T_Data_RGBA_Ref);

	---------------------------------------------------------------------------------------------------------------------
	procedure Create_File_Path (Sanitised_File_Path : in String);

	---------------------------------------------------------------------------------------------------------------------
	function Sanitise_File_Path (File_Path : in String) return String;

	---------------------------------------------------------------------------------------------------------------------
	function Sanitise_Region (
		Region : in T_Region;
		Width  : in T_Size;
		Height : in T_Size
	) return T_Region;



end Cre8or_Generic_Image_Handling;

namespace Dotpack

import Ionic.Zip
import Ionic.Zlib
import Mono.Cecil
import SevenZip
import SevenZip.Compression.LZMA
import System
import System.Collections.Generic
import System.IO
import System.Text
import System.Xml

[Boo.Lang.ExtensionAttribute]
static def ExtractBytes(entry as ZipEntry) as (byte):
	ms = MemoryStream()
	entry.Extract(ms)
	return ms.ToArray()

def ReplaceInt(data as (byte), orig as int, new as int):
	for i in range(data.Length-3):
		if (
				data[i] == (orig & 0xFF) and data[i+1] == ((orig >> 8) & 0xFF) and 
				data[i+2] == ((orig >> 16) & 0xFF) and data[i+3] == (orig >> 24)
			):
			data[i] = new & 0xFF
			data[i+1] = (new >> 8) & 0xFF
			data[i+2] = (new >> 16) & 0xFF
			data[i+3] = new >> 24

abstract class Packer:
	abstract def Pack(infile as string, outfile as string):
		pass
	
	Names as Hash
	def GetName(name as string) as string:
		if not Names.ContainsKey(name):
			i = Names.Count
			Names[name] = ''
			while i != 0:
				Names[name] += cast(char, i & 0xFF).ToString()
				i >>= 8
		return Names[name]
	
	def StripType(type as TypeDefinition):
		type.Namespace = GetName(type.Namespace)
		type.Name = GetName(type.Name)
		for method as MethodDefinition in type.Methods:
			method.Name = GetName(method.Name)
			for parameter as ParameterDefinition in method.Parameters:
				parameter.Name = GetName(parameter.Name)
		for field as FieldDefinition in type.Fields:
			field.Name = GetName(field.Name)
		for prop as PropertyDefinition in type.Properties:
			prop.Name = GetName(prop.Name)
		for evt as EventDefinition in type.Events:
			evt.Name = GetName(evt.Name)
		//if type.HasNestedTypes:
		//	for ntype as TypeDefinition in type.NestedTypes:
		//		StripType(ntype)
	
	def StripNames(asm as AssemblyDefinition):
		Names = {}
		for module as ModuleDefinition in asm.Modules:
			for type as TypeDefinition in module.Types:
				StripType(type)

class SilverlightPacker(Packer):
	class AppManifest:
		Doc as XmlDocument
		
		public EntryPointAssembly as string
		public EntryPointType as string
		
		public Assemblies as Dictionary [of string, string]
		
		def constructor(zip as ZipFile):
			xml = zip['AppManifest.xaml'].ExtractBytes()
			Doc = XmlDocument()
			Doc.Load(MemoryStream(xml))
			
			root = Doc.GetElementsByTagName('Deployment')[0]
			EntryPointAssembly = root.Attributes['EntryPointAssembly'].InnerText
			EntryPointType = root.Attributes['EntryPointType'].InnerText
			
			Assemblies = Dictionary [of string, string]()
			parts = Doc.GetElementsByTagName('Deployment.Parts')[0]
			part = parts.FirstChild
			while part != null:
				Assemblies[part.Attributes['x:Name'].InnerText] = part.Attributes['Source'].InnerText
				part = part.NextSibling
	
	LZMAProperties = (
			1 << 27, 1, 4, 0, 2, 128, 'bt2', false
		)
	
	def Pack(infile as string, outfile as string):
		inzip = ZipFile.Read(infile)
		manifest = AppManifest(inzip)
		
		abytes = inzip[manifest.Assemblies[manifest.EntryPointAssembly]].ExtractBytes()
		
		inasm = AssemblyFactory.GetAssembly(abytes)
		
		asm = AssemblyFactory.GetAssembly('Obj/Stage1.Silverlight.LZMA.dll')
		asm.Name.Name = inasm.Name.Name
		
		stage1 as (byte)
		AssemblyFactory.SaveAssembly(asm, stage1)
		
		msin = MemoryStream(abytes)
		msout = MemoryStream()
		encoder = SevenZip.Compression.LZMA.Encoder()
		propIDs = (
				CoderPropID.DictionarySize,
				CoderPropID.PosStateBits,
				CoderPropID.LitContextBits,
				CoderPropID.LitPosBits,
				CoderPropID.Algorithm,
				CoderPropID.NumFastBytes,
				CoderPropID.MatchFinder,
				CoderPropID.EndMarker
			)
		
		encoder.SetCoderProperties(propIDs, LZMAProperties)
		encoder.Code(msin, msout, -1, -1, null)
		cdata = msout.ToArray()
		
		ReplaceInt(stage1, 0x5EADBEE0, abytes.Length)
		ReplaceInt(stage1, 0x5EADBEE1, cdata.Length)
		ReplaceInt(stage1, 0x4AFEBAB0, LZMAProperties[0]) # Dictionary size
		ReplaceInt(stage1, 0x4AFEBAB1, LZMAProperties[3]) # Literal position bits
		ReplaceInt(stage1, 0x4AFEBAB2, LZMAProperties[2]) # Literal context bits
		ReplaceInt(stage1, 0x4AFEBAB3, 1 << cast(int, LZMAProperties[1])) # Position state bits
		ReplaceInt(stage1, 0x4AFEBAB4, (1 << cast(int, LZMAProperties[3])) - 1) # Literal position state mask
		ReplaceInt(stage1, 0x4AFEBAB5, (1 << cast(int, LZMAProperties[1])) - 1) # Position state mask
		
		#for assembly in manifest.Assemblies:
		#	inzip.RemoveEntry(assembly.Value)
		
		inzip.UpdateEntry(manifest.Assemblies[manifest.EntryPointAssembly], stage1)
		inzip.UpdateEntry('bin', cdata)
		inzip.UpdateEntry('name', UTF8Encoding().GetBytes(manifest.EntryPointType))
		inzip.Save(outfile)

class ExecutablePacker(Packer):
	def Pack(infile as string, outfile as string):
		name, kind, hasParams, sizes as (int), stage2, data2 = CreateStage2(infile, 'LZMA')
		print 'Real assembly: {0} -> {1}' % (sizes[0], sizes[1])
		overhead = CreateStage1(name, kind, hasParams, stage2, sizes[0], data2, outfile)

		total = overhead + sizes[1]
		print 'Total size: {0} -> {1} ({2}%)' % (sizes[0], total, (cast(single, total) / sizes[0]) * 100)
		print 'Total unpacker overhead: {0} ({1}%)' % (overhead, (cast(single, overhead) / total) * 100)
		
	def CreateStage1(name as string, kind as AssemblyKind, hasParams as bool, stage2 as (byte), data2Size as int, data2 as (byte), outfile as string):
		if hasParams:
			p = '.Params'
		else:
			p = ''
		fn = 'Obj/Stage1.Deflate' + p + '.exe'
		asm = AssemblyFactory.GetAssembly(fn)
		asm.Name.Name = name
		asm.Kind = kind
		#Mono.Cecil.Binary.PEOptionalHeader.NTSpecificFieldsHeader.DefaultFileAlignment = 0x200
		StripNames(asm)
		
		stage1 as (byte)
		AssemblyFactory.SaveAssembly(asm, stage1)
		size = stage1.Length
		while stage1[size-1] == 0:
			size -= 1
		print 'Stage 1 size: {0} -> {1}' % (stage1.Length, size)
		s2size = stage2.Length
		#while stage2[s2size-1] == 0:
		#	s2size -= 1
		start = 0
		while stage1[start] == stage2[start]:
			start += 1
		
		ms = MemoryStream()
		cs = DeflateStream(ms, CompressionMode.Compress, CompressionLevel.BestCompression)
		cs.Write(stage2, start, s2size-start)
		cs.Close()
		stage2 = ms.ToArray()
		print 'Stage 2 size: {0} -> {1}' % (s2size, stage2.Length)
		
		ReplaceInt(stage1, 0x5EADBEE0, s2size)
		ReplaceInt(stage1, 0x5EADBEE1, data2Size)
		ReplaceInt(stage1, 0x5EADBEE2, size)
		ReplaceInt(stage1, 0x5EADBEE3, stage2.Length)
		ReplaceInt(stage1, 0x5EADBEE4, data2.Length)
		ReplaceInt(stage1, 0x5EADBEE5, start)
		
		fp = File.Create(outfile)
		fp.Write(stage1, 0, size)
		fp.Write(stage2, 0, stage2.Length)
		fp.Write(data2, 0, data2.Length)
		fp.Close()
		
		return size + stage2.Length
	
	LZMAProperties = (
			1 << 27, 1, 4, 0, 2, 128, 'bt2', false
		)
	
	def CreateStage2(infile as string, compression as string) as Boo.Lang.List:
		asm = AssemblyFactory.GetAssembly(infile)
		kind = asm.Kind
		name = asm.Name.Name
		ep = asm.EntryPoint
		hasParams = ep != null and ep.Parameters != null and ep.Parameters.Count != 0
		sizes = array [of int](2)
		fp = File.OpenRead(infile)
		data = array [of byte](fp.Length)
		fp.Read(data, 0, data.Length)
		if compression == 'Deflate':
			ms = MemoryStream()
			cs = DeflateStream(ms, CompressionMode.Compress, CompressionLevel.BestCompression)
			cs.Write(data, 0, data.Length)
			cs.Close()
			cdata = ms.ToArray()
		elif compression == 'LZMA':
			msin = MemoryStream(data, 0, data.Length)
			msout = MemoryStream()
			encoder = SevenZip.Compression.LZMA.Encoder()
			propIDs = (
					CoderPropID.DictionarySize,
					CoderPropID.PosStateBits,
					CoderPropID.LitContextBits,
					CoderPropID.LitPosBits,
					CoderPropID.Algorithm,
					CoderPropID.NumFastBytes,
					CoderPropID.MatchFinder,
					CoderPropID.EndMarker
				)
			
			encoder.SetCoderProperties(propIDs, LZMAProperties)
			encoder.Code(msin, msout, -1, -1, null)
			cdata = msout.ToArray()
		else:
			cdata = null
		sizes[0] = data.Length
		sizes[1] = cdata.Length
		
		asm = AssemblyFactory.GetAssembly('Obj/Stage2.' + compression + '.dll')
		asm.Name.Name = '.'
		mod = asm.MainModule
		for type as TypeDefinition in mod.Types:
			if type.Name == '_':
				asm.EntryPoint = type.Methods[0]
		StripNames(asm)
		
		#Mono.Cecil.Binary.PEOptionalHeader.NTSpecificFieldsHeader.DefaultFileAlignment = 512
		binary as (byte)
		AssemblyFactory.SaveAssembly(asm, binary)
		if compression == 'LZMA':
			ReplaceInt(binary, 0x4AFEBAB0, LZMAProperties[0]) # Dictionary size
			ReplaceInt(binary, 0x4AFEBAB1, LZMAProperties[3]) # Literal position bits
			ReplaceInt(binary, 0x4AFEBAB2, LZMAProperties[2]) # Literal context bits
			ReplaceInt(binary, 0x4AFEBAB3, 1 << cast(int, LZMAProperties[1])) # Position state bits
			ReplaceInt(binary, 0x4AFEBAB4, (1 << cast(int, LZMAProperties[3])) - 1) # Literal position state mask
			ReplaceInt(binary, 0x4AFEBAB5, (1 << cast(int, LZMAProperties[1])) - 1) # Position state mask
		
		return [name, kind, hasParams, sizes, binary, cdata]

class Dotpack:
	def constructor(args as (string)):
		if args.Length != 2:
			Usage()
			return
		
		infile, outfile = args
		packer as Packer
		if infile.EndsWith('.xap'):
			packer = SilverlightPacker()
		else:
			packer = ExecutablePacker()
		packer.Pack(infile, outfile)
	
	def Usage():
		print 'dotpack.exe [infile] [outfile]'

Dotpack(argv)
